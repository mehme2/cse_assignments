#ifndef _SHELL_H
#define _SHELL_H

#include <sstream>
#include "link.h"
#include "buffer.h"
#include "directory.h"

class Shell {
public:
    Shell();
    void run();
    void ls(const std::vector<std::string> & args) const;
    void mkdir(const std::vector<std::string> & args);
    void rm(const std::vector<std::string> & args);
    void cp(const std::vector<std::string> & args);
    void link(const std::vector<std::string> & args);
    void cd(const std::vector<std::string> & args);
    void cat(const std::vector<std::string> & args) const;
    void save();
    void backup();
    void restore();

private:
    static constexpr size_t FILE_SIZE = 10 * 1024 * 1024;
    Directory * root;
    Directory * current;
    Directory * rootBackup;
};

#endif
