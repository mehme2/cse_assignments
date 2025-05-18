#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

#define FIFO_1_PATH "fifo1"
#define FIFO_2_PATH "fifo2"

int larger(int a, int b) {
    return a > b ? a : b;
}

int smaller(int a, int b) {
    return a < b ? a : b;
}

int sum(int a, int b) {
    return a + b;
}

int diff(int a, int b) {
    return a - b;
}

int product(int a, int b) {
    return a * b;
}

const struct {
    const char * name;
    int (*func)(int, int);
} commands[] = {
    {"larger", &larger},
    {"smaller", &smaller},
    {"sum", &sum},
    {"diff", &diff},
    {"product", &product},
};

const int nCommands = sizeof(commands) / sizeof(commands[0]);

int getCurrentTimestamp(char * buf, int n) {
    time_t t = time(0);
    return strftime(buf, n, "[%Y-%m-%d %H:%M:%S]", localtime(&t));
}

int child1() {
    int numbers[2];         // input numbers
    int results[nCommands]; // result of every command (calculating every single command because assumed that child1 cannot read fifo2 (where the command is sent to))

    char timestamp[32];

    // read input numbers from fifo1
    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 1]: Opening fifo1 to read...\n", timestamp);
    int fifo1Read = open(FIFO_1_PATH, O_RDONLY);
    if(fifo1Read == -1) {
        getCurrentTimestamp(timestamp, sizeof(timestamp));
        printf("%s [Child 1]: ERROR: open\n", timestamp);
        return -1;
    }
    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 1]: Reading numbers from fifo1...\n", timestamp);
    read(fifo1Read, numbers, sizeof(numbers));
    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 1]: Numbers are: %d %d\n", timestamp, numbers[0], numbers[1]);

    // calculate every commands result
    for(int i = 0; i < nCommands; ++i) {
        results[i] = commands[i].func(numbers[0], numbers[1]);
    }

    // write the results to fifo2
    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 1]: Opening fifo2 to write...\n", timestamp);
    int fifo2Write = open(FIFO_2_PATH, O_WRONLY);
    if(fifo2Write == -1) {
        getCurrentTimestamp(timestamp, sizeof(timestamp));
        printf("%s [Child 1]: ERROR: open\n", timestamp);
        close(fifo1Read);
        return -1;
    }
    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 1]: Writing results to fifo2...\n", timestamp);
    write(fifo2Write, results, sizeof(results));

    close(fifo1Read);
    close(fifo2Write);

    return 0;
}

int child2() {
    int command; // input command
    int results[nCommands];

    char timestamp[32];

    // read the command and results
    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 2]: Opening fifo2 to read...\n", timestamp);
    int fifo2Read = open(FIFO_2_PATH, O_RDONLY);
    if(fifo2Read == -1) {
        getCurrentTimestamp(timestamp, sizeof(timestamp));
        printf("%s [Child 2]: ERROR: open\n", timestamp);
        return -1;
    }
    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 2]: Reading command from fifo2...\n", timestamp);
    read(fifo2Read, &command, sizeof(command));

    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 2]: Command is %d (%s)\n", timestamp, command, commands[command].name);

    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 2]: Reading results from fifo2...\n", timestamp);
    read(fifo2Read, results, sizeof(results));

    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 2]: Results are:", timestamp);
    for(int i = 0; i < nCommands; ++i) {
        printf(" %d(%s),", results[i], commands[i].name);
    }
    printf("\n");

    // select the appropriate result and print it
    getCurrentTimestamp(timestamp, sizeof(timestamp));
    printf("%s [Child 2]: Result of %s operation is: %d\n", timestamp, commands[command].name, results[command]);

    close(fifo2Read);

    return 0;
}

int daemonRun;

void daemonHandler(int sig) {
    char timestamp[32];
    switch(sig) {
        case SIGTERM:
            getCurrentTimestamp(timestamp, sizeof(timestamp));
            printf("%s SIGTERM Caught\n", timestamp);
            daemonRun = 0;
            break;
        case SIGHUP:
            getCurrentTimestamp(timestamp, sizeof(timestamp));
            printf("%s SIGHUP Caught\n", timestamp);
            break;
        case SIGUSR1:
            getCurrentTimestamp(timestamp, sizeof(timestamp));
            printf("%s SIGUSR1 Caught\n", timestamp);
            break;
    }
}

int closeCounter = 0;
int logPipeHandler;
pid_t pidChild1, pidChild2;

void parentHandler(int sig) {
    char timestamp[32];
    if(sig == SIGCHLD) {
        int status;
        pid_t pid;

        while((pid = waitpid(-1, &status, WNOHANG)) > 0) {
            getCurrentTimestamp(timestamp, sizeof(timestamp));
            dprintf(logPipeHandler, "%s PID %d", timestamp, pid);
            if(pid == pidChild1) {
                closeCounter++;
                dprintf(logPipeHandler, " (Child 1)");
            }
            if(pid == pidChild2) {
                closeCounter++;
                dprintf(logPipeHandler, " (Child 2)");
            }
            dprintf(logPipeHandler, " exited with status %d.\n", WEXITSTATUS(status));
        }
    }

    if(sig == SIGINT) {
        getCurrentTimestamp(timestamp, sizeof(timestamp));
        dprintf(logPipeHandler, "%s SIGINT received, terminating children...\n", timestamp);
        kill(pidChild1, SIGTERM);
        kill(pidChild2, SIGTERM);
    }
}

int main(int argc, char ** argv) {
    if(argc < 4) {
        printf("Arguments: [command] [number 1] [number 2]\n");
        printf("Commands: ");
        for(int i = 0; i < nCommands; ++i) {
            printf("%s ",commands[i].name);
        }
        printf("\n");
        return 0;
    }

    int command = -1;

    for(int i = 0; i < nCommands; ++i) {
        if(strcmp(argv[1], commands[i].name) == 0) {
            command = i;
            break;
        }
    }

    if(command == -1) {
        printf("Invalid command.\n");
        return -1;
    }

    int numbers[2] = {atoi(argv[2]), atoi(argv[3])};

    // disable buffering
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);

    // create fifos
    if(mkfifo(FIFO_1_PATH, 0666) == -1 || mkfifo(FIFO_2_PATH, 0666) == -1) {
        perror("ERROR: mkfifo");
        return -1;
    }

    // pipe to get error logs from children
    int logPipe[2];
    pipe(logPipe);

    // create children
    pidChild1 = fork();

    if(pidChild1 == -1) {
        perror("ERROR: fork");
        return -1;
    }
    else if(pidChild1 == 0) {
        sleep(10);
        close(logPipe[0]);
        dup2(logPipe[1], STDOUT_FILENO);
        dup2(logPipe[1], STDERR_FILENO);
        close(logPipe[1]);
        return child1();
    }

    pidChild2 = fork();

    if(pidChild2 == -1) {
        perror("ERROR: fork");
        return -1;
    }
    else if(pidChild2 == 0) {
        sleep(10);
        close(logPipe[0]);
        dup2(logPipe[1], STDOUT_FILENO);
        dup2(logPipe[1], STDERR_FILENO);
        close(logPipe[1]);
        return child2();
    }

    pid_t pidDaemon = fork();

    if(pidDaemon == -1) {
        perror("ERROR: fork");
        return -1;
    }
    else if(pidDaemon == 0) {
        close(logPipe[1]);

        if(setsid() == -1) {
            perror("ERROR: setsid");
            return -1;
        }

        switch(fork()) {
            case 0:
                break;
            case -1:
                perror("ERROR: fork");
                return -1;
            default:
                return 0;
        }

        umask(0);

        // close/replace standard fds
        int logfd = open("log.txt", O_WRONLY | O_CREAT | O_APPEND, 0644);
        dup2(logfd, STDOUT_FILENO);
        dup2(logfd, STDERR_FILENO);
        close(logfd);
        close(STDIN_FILENO);

        struct sigaction sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_handler = daemonHandler;

        sigaction(SIGTERM, &sa, 0);
        sigaction(SIGHUP, &sa, 0);
        sigaction(SIGUSR1, &sa, 0);

        char timestamp[32];
        getCurrentTimestamp(timestamp, sizeof(timestamp));
        printf("%s Started Daemon\n", timestamp);

        // read handles to monitor ipc
        int fifo1Read = open(FIFO_1_PATH, O_RDONLY | O_NONBLOCK);
        int fifo2Read = open(FIFO_2_PATH, O_RDONLY | O_NONBLOCK);

        daemonRun = 1;
        while(daemonRun) {
            // check fifos for errors
            int dummy;
            if(read(fifo1Read, &dummy, 0) != 0) {
                getCurrentTimestamp(timestamp, sizeof(timestamp));
                printf("%s ERROR: read fifo1", timestamp);
            }
            if(read(fifo2Read, &dummy, 0) != 0) {
                getCurrentTimestamp(timestamp, sizeof(timestamp));
                printf("%s ERROR: read fifo2", timestamp);
            }

            // check log pipe and print if anything is sent
            char buf[256];
            int n = read(logPipe[0], buf, sizeof(buf));
            switch(n) {
                case 0:  // EOF (write ends are closed)
                    daemonRun = 0;
                    break;
                case -1: // empty non-block pipes return -1
                    break;
                default:
                    write(STDOUT_FILENO, buf, n);
                    break;
            }
        }

        getCurrentTimestamp(timestamp, sizeof(timestamp));
        printf("%s Closing Daemon...\n", timestamp);

        close(logPipe[0]);
        close(fifo1Read);
        close(fifo2Read);

        // close log.txt
        close(STDOUT_FILENO);
        close(STDERR_FILENO);

        return 0;
    }

    close(logPipe[0]);

    logPipeHandler = logPipe[1];

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = parentHandler;

    sigaction(SIGCHLD, &sa, 0);
    sigaction(SIGINT, &sa, 0);

    // send numbers and command to fifos

    int fifo1 = open(FIFO_1_PATH, O_RDWR);
    write(fifo1, numbers, sizeof(numbers));

    int fifo2 = open(FIFO_2_PATH, O_RDWR);
    write(fifo2, &command, sizeof(command));

    while(closeCounter < 2) {
        printf("proceeding...\n");
        sleep(2);
    }

    close(logPipe[1]);
    close(fifo1);
    close(fifo2);

    unlink(FIFO_1_PATH);
    unlink(FIFO_2_PATH);

    return 0;
}
