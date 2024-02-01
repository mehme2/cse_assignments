#ifndef _BUFFER_H
#define _BUFFER_H

#include <vector>
#include "file.h"

class Buffer : public File {
public:
    Buffer(const std::string & n, const std::vector<char> & d = {});
    Buffer(std::ifstream & file);
    Buffer(const Buffer & o);
    const Buffer & operator=(const Buffer & o);
    ~Buffer();
    virtual size_t getSize() const override;
    virtual void cat() const override;
    virtual File * cp() const override;
    virtual void save(std::ostream & file) const override;
    const std::vector<char> & getData() const;
    void setData(const std::vector<char> & d);
    std::vector<char>::iterator begin();
    std::vector<char>::iterator end();

private:
    std::vector<char> data;
};

#endif
