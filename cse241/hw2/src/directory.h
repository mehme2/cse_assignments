#ifndef _DIRECTORY_H
#define _DIRECTORY_H

#include <vector>
#include "file.h"

class Directory : public File {
public:
    Directory(const std::string & n);
    Directory(std::ifstream & file);
    Directory(const Directory & o);
    const Directory & operator=(const Directory & o);
    ~Directory();
    virtual size_t getSize() const override;
    virtual void cat() const override;
    virtual File * cp() const override;
    virtual void save(std::ostream & file) const override;
    void ls(bool recursive = false, const std::string & ref = ".") const;
    File * get(const std::string & path);
    void insert(File * f);
    void erase(const std::string & name);
    bool empty() const;
    const std::string getPath() const;
    std::vector<File*>::iterator begin();
    std::vector<File*>::iterator end();

private:
    std::vector <File*> files;
    Directory * parent;
};

#endif
