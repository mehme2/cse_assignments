#include "directory.h"
#include "buffer.h"
#include "link.h"
#include <iomanip>

Directory::Directory(const std::string & n) : File(n), parent(nullptr) { /*empty*/ }

Directory::Directory(std::ifstream & file) : File(file) {
    size_t size;
    file >> size;
    file.get();
    for(size_t i = 0; i < size; ++i) {
        char sym;
        file >> sym;
        file.get();
        File * nf;
        switch(sym) {
            case 'D':
                nf = new Directory(file);
                break;
            case 'B':
                nf = new Buffer(file);
                break;
            case 'L':
                nf = new Link(file);
                break;
            default:
                throw std::invalid_argument("invalid partition");
        }
        insert(nf);
    }
}

Directory::Directory(const Directory & o) : File(o), parent(nullptr) {
    for(auto f : o.files) {
        insert(f->cp());
    }
}

const Directory & Directory::operator=(const Directory & o) {
    if(this != &o) {
        files.clear();
        parent = nullptr;
        for(auto f : o.files) {
            insert(f->cp());
        }
        updateTime();
    }
    return *this;
}

Directory::~Directory() {
    for(auto f : files)
        delete f;
}

size_t Directory::getSize() const {
    size_t size = 0;
    for(auto f : files)
        size += f->getSize();
    return size;
}

void Directory::cat() const {
    throw std::invalid_argument("file is a directory");
}

File * Directory::cp() const {
    return new Directory(*this);
}

void Directory::save(std::ostream & file) const {
    file << "D\n";
    File::save(file);
    file << files.size() << '\n';
    for(auto f : files)
        f->save(file);
}

void Directory::ls(bool recursive, const std::string & ref) const {
    if(recursive)
        std::cout << ref << ":\n";
    std::cout << "D " << std::setw(8) << getSize() << std::setw(18) << getTime() << " .\n";
    if(parent != nullptr)
        std::cout << "D " << std::setw(8) << parent->getSize() << std::setw(18) << parent->getTime() << " ..\n";
    for(auto f : files) {
        auto lp = dynamic_cast<Link*>(f);
        auto bp = dynamic_cast<Buffer*>(f);
        auto dp = dynamic_cast<Directory*>(f);
        if(lp != nullptr)
            std::cout << "L ";
        if(bp != nullptr)
            std::cout << "B ";
        if(dp != nullptr)
            std::cout << "D ";
        std::cout << std::setw(8) << f->getSize() << std::setw(18) << f->getTime() << " " << f->getName();
        if(lp != nullptr)
            std::cout << " -> " << lp->getPath();
        std::cout << '\n';
    }
    if(recursive) {
        for(auto f : files) {
            auto dp = dynamic_cast<Directory*>(f);
            if(dp != nullptr) {
                std::cout << '\n';
                dp->ls(true, ref + "/" + dp->getName());
            }
        }
    }
}

File * Directory::get(const std::string & path) {
    if(path == "" || path == ".")
        return this;
    if(path == "..")
        return parent;
    int ns = path.find('/');
    if(ns != path.npos) {
        auto dp = dynamic_cast<Directory*>(this->get(path.substr(0, ns)));
        if(dp != nullptr)
            return dp->get(path.substr(ns + 1));
        return nullptr;
    }
    for(auto f : *this)
        if(f->getName() == path) {
            auto link = dynamic_cast<Link*>(f);
            if(link != nullptr)
                return get(link->getPath());
            return f;
        }
    return nullptr;
}

void Directory::insert(File * f) {
    auto p = this->get(f->getName());
    if(p != nullptr) {
        delete f;
        throw std::invalid_argument("file with the same name already exists");
    }
    auto dp = dynamic_cast<Directory*>(f);
    if(dp != nullptr)
        dp->parent = this;
    auto lp = dynamic_cast<Link*>(f);
    files.push_back(f);
}

void Directory::erase(const std::string & name) {
    for(int i = 0; i < files.size(); ++i) {
        if(name == files[i]->getName()) {
            delete files[i];
            files.erase(files.begin() + i);
            --i;
        }
    }
}

bool Directory::empty() const {
    return files.empty();
}

const std::string Directory::getPath() const {
    if(parent != nullptr)
        return parent->getPath() + "/" + getName();
    else
        return getName();
}

std::vector<File*>::iterator Directory::begin() {
    return files.begin();
}

std::vector<File*>::iterator Directory::end() {
    return files.end();
}
