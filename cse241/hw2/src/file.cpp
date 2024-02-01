#include "file.h"
#include <stdexcept>

File::File(const std::string & n) : name(n) {
    if(name.find('/') != name.npos)
        throw std::invalid_argument("invalid file name");
    updateTime();
}

File::File(std::ifstream & file) {
    std::getline(file, name);
    file >> time;
    file.get();
}

File::File(const File & o) : File(o.name) { /* empty */ }

const File & File::operator=(const File & o) {
    name = o.name;
    updateTime();
    return *this;
}

File::~File() = default;

void File::save(std::ostream & file) const {
    file << name << '\n' << time << '\n';
}

void File::setName(const std::string & n) {
    name = n;
}

const std::string & File::getName() const {
    return name;
}

const std::string File::getTime() const {
    char buf[50];
    char fileYear[10];
    char curYear[10];
    auto cur = std::time(nullptr);
    std::strftime(fileYear, sizeof(fileYear), "%Y", std::localtime(&time));
    std::strftime(curYear, sizeof(curYear), "%Y", std::localtime(&cur));
    if(std::string(fileYear) != curYear)
        std::strftime(buf, sizeof(buf), "%Y %b %d %H:%M", std::localtime(&time));
    else
        std::strftime(buf, sizeof(buf), "%b %d %H:%M", std::localtime(&time));
    return buf;
}

void File::updateTime() {
    time = std::time(nullptr);
}
