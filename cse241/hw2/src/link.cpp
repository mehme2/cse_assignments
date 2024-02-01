#include "link.h"

Link::Link(const std::string & n, const std::string & p) : File(n), path(p) { /*empty*/ }

Link::Link(std::ifstream & file) : File(file) {
    std::getline(file, path);
}

Link::Link(const Link & o) : File(o) , path(o.path) { /*empty*/ }

const Link & Link::operator=(const Link & o) {
    File::operator=(o);
    path = o.path;
    return *this;
}

Link::~Link() = default;

size_t Link::getSize() const {
    return path.size();
}

void Link::cat() const {
    std::cout << path;
}

File * Link::cp() const {
    return new Link(*this);
}

void Link::save(std::ostream & file) const {
    file << "L\n";
    File::save(file);
    file << path << '\n';
}

const std::string & Link::getPath() const {
    return path;
}

std::string::iterator Link::begin() {
    return path.begin();
}

std::string::iterator Link::end() {
    return path.end();
}
