#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include <sys/wait.h>
#include <dirent.h>

// to make the function easier to read
#define LOG_AND_PRINT(...) logAndPrint((const char *[]){__VA_ARGS__, NULL})

// to avoid writing the same default message on every command
#define DEFAULT_ERROR default: LOG_AND_PRINT("Error: An error occurred."); break;

// writes the formatted timestamp to the given fd
void writeCurrentTimestamp(int fd) {
    char buf[32];
    time_t t = time(0);
    int n = strftime(buf, sizeof(buf), "[%Y-%m-%d %H:%M:%S]", localtime(&t));
    write(fd, buf, n);
}

// writes given message array by concatenating to log and and stdout
// input: null-terminated array of char pointers
void logAndPrint(const char ** msg) {
    // append to log.txt and create it if it does not exist
    // create with same permissions as touch command in core utils
    int logfd = open("log.txt", O_APPEND | O_WRONLY | O_CREAT, 0644);

    // start the log.txt with the timestamp
    writeCurrentTimestamp(logfd);
    write(logfd, " ", 1);

    // concatenate every message part by writing them in a loop
    while(msg[0] != NULL) {
        int len = strlen(msg[0]);

        write(1, msg[0], len);
        write(logfd, msg[0], len);

        msg++;
    }

    write(1, "\n", 1);
    write(logfd, "\n", 1);

    close(logfd);
}

// returns the nth argument after the command
// exits with status -1 if there are not enough arguments given
char * getArg(int argc, char ** argv, int n) {
    if(argc < 2 + n) {
        LOG_AND_PRINT("Error: Invalid number of arguments.");
        exit(-1);
    }

    return argv[1 + n];
}

// create a directory with first argument as name
int createDir(int argc, char ** argv) {
    char * dirName = getArg(argc, argv, 1);
    
    // same permissions as mkdir command in core utils
    int res = mkdir(dirName, 0755);

    if(res == -1) {
        switch(errno) {
            case EEXIST:
                LOG_AND_PRINT("Error: \"", dirName, "\" already exists.");
                break;

                DEFAULT_ERROR
        }

        return -1;
    }

    LOG_AND_PRINT("Directory \"", dirName, "\" created successfully.");

    return 0;
}

// create a file with first argument as name
int createFile(int argc, char ** argv) {
    char * fileName = getArg(argc, argv, 1);

    // same permissions as touch command in core utils
    int fd = open(fileName, O_CREAT | O_EXCL | O_WRONLY, 0644);

    if(fd == -1) {
        switch(errno) {
            case EEXIST:
                LOG_AND_PRINT("Error: \"", fileName, "\" already exists.");
                break;

            DEFAULT_ERROR
        }

        return -1;
    }

    writeCurrentTimestamp(fd);

    close(fd);

    LOG_AND_PRINT("File \"", fileName, "\" created successfully.");

    return 0;
}

// called by listDir and listByExtension
// creates a child process and lists files with the given extension in the given directory
// lists every file if extension is an empty string
int listFilesByExtensionSub(const char * dirName, const char * extension) {
    int pid = fork();

    if(pid == 0) {

        DIR * dir = opendir(dirName);

        if(!dir) {
            switch(errno) {
                case ENOENT:
                    LOG_AND_PRINT("Error: Directory \"", dirName, "\" not found.");
                    break;

                DEFAULT_ERROR
            }

            return -1;
        }

        // number of files with matching extension
        int nMatches = 0;

        int extensionLength = strlen(extension);

        // the next directory entry
        struct dirent * next;

        // check every file name and print if match
        while((next = readdir(dir)) != NULL) {
            int nameLength = strlen(next->d_name);
            int compareIndex = 0;

            // skip if file name is shorter than extension
            if(nameLength < extensionLength) continue;

            // start from file name length - extension length and check until last char
            while(compareIndex < extensionLength && extension[compareIndex] == next->d_name[nameLength - extensionLength + compareIndex]) compareIndex++;

            // comparison quits at index 0 when extension is empty so it lists every entry
            if(compareIndex == extensionLength) {
                write(1, next->d_name, strlen(next->d_name));
                write(1, "\n", 1);

                nMatches++;
            }
        }

        if(nMatches == 0) {
            LOG_AND_PRINT("No files with extension \"", extension, "\" found in \"", dirName, "\".");
        }

        closedir(dir);

        LOG_AND_PRINT("Directory \"", dirName, "\" listed successfully.");
    }
    else {
        // wait for child to exit and return its status
        int status;
        waitpid(pid, &status, 0);
        return status;
    }

    return 0;
}

// lists every file in directory with first argument as name
int listDir(int argc, char ** argv) {
    return listFilesByExtensionSub(getArg(argc, argv, 1), "");
}

// lists every file with same extension as second argument in directory with first argument as name
int listFilesByExtension(int argc, char ** argv) {
    return listFilesByExtensionSub(getArg(argc, argv, 1), getArg(argc, argv, 2));
}

// prints contents of file in first argument
int readFile(int argc, char ** argv) {
    char * fileName = getArg(argc, argv, 1);

    int fd = open(fileName, O_RDONLY);

    if(fd == -1) {
        switch(errno) {
            case ENOENT:
                LOG_AND_PRINT("Error: File \"", fileName, "\" not found.");
                break;

            DEFAULT_ERROR
        }

        return -1;
    }

    // buffer to hold parts of the file text
    char buf[256];
    int nRead;

    // read and print the contents part by part until EOF is reached
    while((nRead = read(fd, buf, sizeof(buf))) != 0) {
        write(1, buf, nRead);
    }

    close(fd);

    LOG_AND_PRINT("File \"", fileName, "\" read successfully.");

    return 0;
}

// appends text given in second argument to file in first argument
int appendToFile(int argc, char ** argv) {
    char * fileName = getArg(argc, argv, 1);
    char * content = getArg(argc, argv, 2);

    int fd = open(fileName, O_APPEND | O_WRONLY);

    if(fd == -1) {
        switch(errno) {
            case ENOENT:
                LOG_AND_PRINT("Error: File \"", fileName, "\" not found.");
                break;

            // open gives this error when the file is read-only
            case EACCES:
                LOG_AND_PRINT("Error: Permission denied.\n");
                break;

            DEFAULT_ERROR
        }

        return -1;
    }

    struct flock fl;
    memset(&fl, 0, sizeof(fl));
    fl.l_type = F_WRLCK;

    // try to write-lock the file
    if(fcntl(fd, F_SETLK, &fl) == -1) {
        switch(errno) {
            // either of these errors represents file is already locked according to the manual
            case EACCES:
            case EAGAIN:
                LOG_AND_PRINT("Error: Cannot write to \"", fileName, "\". File is locked.");
                close(fd);
                break;

            DEFAULT_ERROR
        }

        return -1;
    }

    write(fd, content, strlen(content));

    fl.l_type = F_UNLCK;

    // unlock the file
    fcntl(fd, F_SETLK, &fl);

    close(fd);

    LOG_AND_PRINT("File \"", fileName, "\" appended succesfully.");

    return 0;
}

// deletes the file in first argument
int deleteFile(int argc, char ** argv) {
    int pid = fork();

    if(pid == 0) {
        char * fileName = getArg(argc, argv, 1);

        if(unlink(fileName) == -1) {
            switch(errno) {
                case ENOENT:
                    LOG_AND_PRINT("Error: File \"", fileName, "\" not found.");
                    break;

                DEFAULT_ERROR
            }

            return -1;
        }

        LOG_AND_PRINT("File \"", fileName, "\" deleted successfully.");
    }
    else {
        // wait for child to exit and return its status
        int status;
        waitpid(pid, &status, 0);
        return status;
    }

    return 0;
}

// deletes the directory in first argument
int deleteDir(int argc, char ** argv) {
    int pid = fork();

    if(pid == 0) {
        char * dirName = getArg(argc, argv, 1);

        if(rmdir(dirName) == -1) {
            switch(errno) {
                case ENOENT:
                    LOG_AND_PRINT("Error: Directory \"", dirName, "\" not found.");
                    break;

                // either of these errors represents directory is not empty according to the manual
                case EEXIST:
                case ENOTEMPTY:
                    LOG_AND_PRINT("Error: Directory \"", dirName, "\" not empty.");
                    break;

                DEFAULT_ERROR
            }

            return -1;
        }

        LOG_AND_PRINT("Directory \"", dirName, "\" deleted successfully.");
    }
    else {
        // wait for child to exit and return its status
        int status;
        waitpid(pid, &status, 0);
        return status;
    }

    return 0;
}

// prints the log file
// takes no arguments
int showLogs(int argc, char ** argv) {
    int fd = open("log.txt", O_RDONLY);

    if(fd == -1) {
        switch(errno) {
            case ENOENT:
                // logAndPrint automatically creates log.txt when it does not exist
                LOG_AND_PRINT("Error: Log file not found. Creating \"log.txt\"...");
                break;

            DEFAULT_ERROR
        }

        return -1;
    }

    // same reading method as readFile
    char buf[256];
    int nRead;

    while((nRead = read(fd, buf, sizeof(buf))) != 0) {
        write(1, buf, nRead);
    }

    close(fd);

    LOG_AND_PRINT("Log file shown successfully.");

    return 0;
}

// structure to hold the command array entries
typedef struct {
    // string name of the command
    char * name;
    // pointer to the command's function, returns 0 on success
    // takes argument array given to main as input
    int (*proc)(int argc, char ** argv);
} command;

int main(int argc, char ** argv) {
    command commands[] = {
		{"createDir", createDir},
		{"createFile", createFile},
		{"listDir", listDir},
		{"listFilesByExtension", listFilesByExtension},
		{"readFile", readFile},
		{"appendToFile", appendToFile},
		{"deleteFile", deleteFile},
		{"deleteDir", deleteDir},
		{"showLogs", showLogs},
    };

    int nCommands = sizeof(commands) / sizeof(command);

    // if no command was entered
    if(argc < 2) {
        char help[] = 
            "Usage: fileManager <command> [arguments]\n"
            "\n"
            "Commands:\n\n"
            " createDir \"folderName\" - Create a new directory\n"
            " createFile \"fileName\" - Create a new file\n"
            " listDir \"folderName\" - List all files in a directory\n"
            " listFilesByExtension \"folderName\" \".txt\" - List files with specific extension\n"
            " readFile \"fileName\" - Read a file's content\n"
            " appendToFile \"fileName\" \"new content\" - Append content to a file\n"
            " deleteFile \"fileName\" - Delete a file\n"
            " deleteDir \"folderName\" - Delete an empty directory\n"
            " showLogs - Display operation logs\n";

        write(1, help, sizeof(help));

        return 0;
    }

    char * selectedCommand = argv[1];

    // compare every command name to the given to the input and call the function on match
    for(int i = 0; i < nCommands; ++i) {
        if(strcmp(commands[i].name, selectedCommand) == 0) {
            return commands[i].proc(argc, argv);
        }
    }

    LOG_AND_PRINT("Error: Invalid command: \"", selectedCommand, "\"");

    return -1;
}
