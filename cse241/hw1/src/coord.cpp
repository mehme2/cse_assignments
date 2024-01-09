#include "coord.h"

Coord::Coord(char file, int rank) : file(file), rank(rank) {/*empty*/};

bool Coord::operator==(const Coord & o) const {
    return (file == o.file) && (rank == o.rank);
}

bool Coord::operator!=(const Coord & o) const{
    return !(*this == o);
}

bool Coord::outOfBounds() const {
    return file > 'h' || file < 'a' || rank > 8 || rank < 1;
}

char Coord::getFile() const {
    return file;
}
int Coord::getRank() const {
    return rank;
}
void Coord::setFile(char f) {
    file = f;
}
void Coord::setRank(int r) {
    rank = r;
}

Move::Move(const Coord & s, const Coord & e, char o) : _start(s), _end(e), _option(o) {/*empty*/}

const Coord & Move::getStart() const {
    return _start;
}

const Coord & Move::getEnd() const {
    return _end;
}

bool Move::operator==(const Move & o) const {
    return (_start == o._start) && (_end == o._end) && (_option == o._option);
}

bool Move::operator!=(const Move & o) const {
    return !(*this == o);
}

std::ostream & operator<<(std::ostream & os, const Move & m) {
    os << m._start.getFile() << m._start.getRank() <<
        m._end.getFile() << m._end.getRank();
    if(m._option != 0)
        os << m._option;
    return os;
}

char Move::getOption() const {
    return _option;
}
