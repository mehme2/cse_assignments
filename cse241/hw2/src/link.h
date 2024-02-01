#ifndef _LINK_H
#define _LINK_H

#include "file.h"

class Link : public File {
public:
    Link(const std::string & n, const std::string & p);
    Link(std::ifstream & file);
    Link(const Link & o);
    const Link & operator=(const Link & o);
    ~Link();
    virtual size_t getSize() const override;
    virtual void cat() const override;
    virtual File * cp() const override;
    virtual void save(std::ostream & file) const override;
    const std::string & getPath() const;
    std::string::iterator begin();
    std::string::iterator end();

private:
    std::string path;
};


#endif
