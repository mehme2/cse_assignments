#ifndef _FILE_H
#define _FILE_H

#include <string>
#include <ctime>
#include <fstream>
#include <iostream>
#include <stdexcept>

class File {
public:
    File(const std::string & n);
    File(std::ifstream & file);
    File(const File & o);
    const File & operator=(const File & o);
    virtual ~File();
    virtual void cat() const = 0;
    virtual File * cp() const = 0;
    virtual size_t getSize() const = 0;
    virtual void save(std::ostream & file) const;
    void setName(const std::string & n);
    const std::string & getName() const;
    const std::string getTime() const;
    void updateTime();

private:
    std::string name;
    std::time_t time;
};


#endif
