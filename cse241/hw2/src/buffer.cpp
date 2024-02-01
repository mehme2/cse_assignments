#include "buffer.h"

Buffer::Buffer(const std::string & n, const std::vector<char> & d) : File(n), data(d) { /*empty*/ }

Buffer::Buffer(std::ifstream & file) : File(file) {
    size_t size;
    file >> size;
    data.resize(size);
    file.read(&data[0], size);
    file.get();
}

Buffer::Buffer(const Buffer & o) : File(o) , data(o.data) { /*empty*/ }

const Buffer & Buffer::operator=(const Buffer & o) {
    File::operator=(o);
    data = o.data;
    return *this;
}

Buffer::~Buffer() = default;

size_t Buffer::getSize() const {
    return data.size();
}

void Buffer::cat() const {
    for(auto b : data)
        std::cout << b;
}

File * Buffer::cp() const {
    return new Buffer(*this);
}

void Buffer::save(std::ostream & file) const {
    file << "B ";
    File::save(file);
    auto size = data.size();
    file << data.size();
    if(size > 0)
        file.write(&data[0], data.size());
    file << '\n';
}

const std::vector<char> & Buffer::getData() const {
    return data;
}

void Buffer::setData(const std::vector<char> & d) {
    updateTime();
    data = d;
}

std::vector<char>::iterator Buffer::begin() {
    return data.begin();
}

std::vector<char>::iterator Buffer::end() {
    return data.end();
}
