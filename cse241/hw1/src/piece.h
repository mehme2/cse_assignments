#include <vector>

#include "coord.h"

class Piece {
public:
    Piece(char t, const Coord & c, bool w);
    void moveTo(const Move & c);
    bool canAttack(const Piece & o, const Coord & move) const;
    const std::vector<Move> getPossibleMoves() const;
    char getType() const;
    const Coord getCoords() const;
    bool isWhite() const;
    float getPoints() const;
private:
    char type;
    Coord coord;
    bool white;
};
