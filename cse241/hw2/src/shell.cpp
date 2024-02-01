#include "shell.h"

Shell::Shell() {
    std::ifstream file("partition", std::ifstream::binary);
    root = nullptr;
    rootBackup = nullptr;
    if(file.is_open()) {
        file.seekg(0, file.end);
        auto size = file.tellg();
        file.seekg(0, file.beg);
        if(size == FILE_SIZE) {
            char sym;
            file >> sym;
            file.get();
            if(sym == 'D') {
                root = new Directory(file);
            }
            else
                std::cout << "partition file is corrupted\n";
        }
        else
            std::cout << "invalid partition file size\n";
        file.close();
    }
    if(root == nullptr)
        root = new Directory("root");
    current = root;
    backup();
}

void Shell::run() {
    bool exit = false;
    while(!exit) {
        std::vector<std::string> args;
        std::string word;
        std::cout << current->getName() << ": ";
        do {
            std::cin >> word;
            args.push_back(word);
        }
        while(std::cin.peek() != '\n');
        try {
            if(args[0] == "ls")
                ls(args);
            else if(args[0] == "cp")
                cp(args);
            else if(args[0] == "mkdir")
                mkdir(args);
            else if(args[0] == "cd")
                cd(args);
            else if(args[0] == "cat")
                cat(args);
            else if(args[0] == "rm")
                rm(args);
            else if(args[0] == "link")
                link(args);
            else if(args[0] == "exit")
                exit = true;
        }
        catch (std::overflow_error & e) {
            std::cout << e.what() << "\nrestoring backup...\n";
            restore();
        }
        catch (std::invalid_argument & e) {
            std::cout << e.what();
        }
        std::cout << '\n';
    }
}

void Shell::ls(const std::vector<std::string> & args) const {
    current->ls(args.size() > 1 && args[1] == "-R");
}

void Shell::mkdir(const std::vector<std::string> & args) {
    if(args.size() < 2)
        throw std::invalid_argument("usage: mkdir [target]");
    const auto & path = args[1];
    int ls = path.rfind('/');
    if(ls == path.npos)
        current->insert(new Directory(path));
    else {
        auto dir = dynamic_cast<Directory*>(current->get(path.substr(0, ls)));
        if(dir == nullptr)
            throw std::invalid_argument("invalid path");
        dir->insert(new Directory(path.substr(ls + 1)));
    }
    save();
}

void Shell::rm(const std::vector<std::string> & args) {
    if(args.size() < 2)
        throw std::invalid_argument("usage: rm [target]");
    const auto & path = args[1];
    int ls = path.rfind('/');
    if(ls == path.npos)
        current->erase(path);
    else {
        auto dir = dynamic_cast<Directory*>(current->get(path.substr(0, ls)));
        if(dir == nullptr)
            throw std::invalid_argument("invalid path");
        dir->erase(path.substr(ls + 1));
    }
    save();
}

void Shell::cp(const std::vector<std::string> & args) {
    if(args.size() < 2)
        throw std::invalid_argument("usage: cp [src] (opt)[dest]");
    const auto & src = args[1];
    const auto & dest = args.size() >= 3 ? args[2] : ".";
    File * copy = nullptr;
    if(src[0] == '/') {
        std::ifstream file(src, std::ifstream::binary);
        if(file.is_open()) {
            std::vector<char> data;
            file.seekg(0, file.end);
            int len = file.tellg();
            data.resize(len);
            file.seekg(0, file.beg);
            file.read(&data[0], len);
            file.close();
            copy = new Buffer(src.substr(src.rfind('/') + 1), data);
        }
    }
    else {
        auto o = current->get(src);
        if(o != nullptr)
            copy = o->cp();
    }
    if(copy == nullptr)
        throw std::invalid_argument("source file not found");
    if(dest[0] == '/') {
        std::ofstream file(dest, std::ostream::binary);
        if(file.is_open()) {
            auto bf = dynamic_cast<Buffer*>(copy);
            if(bf != nullptr)
                file.write(&bf->getData()[0], bf->getData().size());
            file.close();
        }
        delete copy;
    }
    else {
        Directory * dir;
        dir = dynamic_cast<Directory*>(current->get(dest));
        if(dir == nullptr) {
            int ls = dest.rfind('/');
            if(ls != dest.npos)
                dir = dynamic_cast<Directory*>(current->get(dest.substr(0, ls)));
            else
                dir = current;
            copy->setName(dest.substr(ls + 1));
        }
        if(dir != nullptr)
            dir->insert(copy);
    }
    save();
}

void Shell::link(const std::vector<std::string> & args) {
    if(args.size() < 3)
        throw std::invalid_argument("usage: link [src] [dest]");
    current->insert(new Link(args[2], args[1]));
    save();
}

void Shell::cd(const std::vector<std::string> & args) {
    if(args.size() < 2)
        current = root;
    else {
        auto f = dynamic_cast<Directory*>(current->get(args[1]));
        if(f != nullptr)
            current = f;
        else
            throw std::invalid_argument("file not found");
    }
}

void Shell::cat(const std::vector<std::string> & args) const {
    if(args.size() < 2)
        throw std::invalid_argument("usage: cat [target]");
    auto f = current->get(args[1]);
    if(f != nullptr)
        f->cat();
    else
        throw std::invalid_argument("file not found");
}

void Shell::save() {
    const size_t TEN_MB = 10 * 1024 * 1024;
    std::stringstream ss;
    root->save(ss);
    if(ss.tellp() > TEN_MB)
        throw std::overflow_error("partition exceeds 10M");
    std::ofstream partition("partition", std::ofstream::binary);
    if(partition.is_open()) {
        partition.seekp(10 * 1024 * 1024 - 1, partition.beg);
        partition << '\0';
        partition.seekp(0, partition.beg);
        partition << ss.str();
        partition.close();
    }
    backup();
}

void Shell::backup() {
    if(rootBackup != nullptr)
        delete rootBackup;
    rootBackup = dynamic_cast<Directory*>(root->cp());
}

void Shell::restore() {
    std::string path = current->getPath();
    delete root;
    root = dynamic_cast<Directory*>(rootBackup->cp());
    int ls = path.find('/');
    current = dynamic_cast<Directory*>(root->get(path.substr(ls + 1)));
    if(current == nullptr)
        current = root;
}
