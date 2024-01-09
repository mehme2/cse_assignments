#include <iostream>

class Coord {
public:
    Coord(char file, int rank);
    bool operator==(const Coord & o) const;
    bool operator!=(const Coord & o) const;
    bool outOfBounds() const;
    char getFile() const;
    int getRank() const;
    void setFile(char f);
    void setRank(int r);
private:
    char file;
    int rank;
};

class Move {
public:
    Move(const Coord & s, const Coord & e, char o = 0);
    const Coord & getStart() const;
    const Coord & getEnd() const;
    bool operator==(const Move & o) const;
    bool operator!=(const Move & o) const;
    char getOption() const;
    friend std::ostream & operator<<(std::ostream & os, const Move & m);
private:
    Coord _start;
    Coord _end;
    char _option;
};
