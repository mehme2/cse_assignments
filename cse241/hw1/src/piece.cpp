#include <iostream>
#include "piece.h"

Piece::Piece(char t, const Coord & c, bool w) : 
    type(t), coord(c), white(w) {/*empty*/}

char Piece::getType() const {
    return type;
}

const Coord Piece::getCoords() const {
    return coord;
}

bool Piece::isWhite() const {
    return white;
}

float Piece::getPoints() const {
    switch(type) {
        case 'p':
            return 1.0f;
        case 'n':
            return 3.0f;
        case 'b':
            return 3.0f;
        case 'r':
            return 5.0f;
        case 'q':
            return 9.0f;
        default:
            return 0.0f;
    }
}

bool Piece::canAttack(const Piece & o, const Coord & move) const {
    switch(type) {
        case 'p':
            if(move.getFile() == coord.getFile())
                return false;
        case 'r':
        case 'n':
        case 'b':
        case 'q':
        case 'k':
            return (o.white != white) && (move == o.getCoords());
    }
    return false;
}

void Piece::moveTo(const Move & move) {
    coord = move.getEnd();
    if(type == 'p' && coord.getRank() == (white ? 8 : 1))
        type = move.getOption();
}

const std::vector<Move> Piece::getPossibleMoves() const {
    std::vector<Move> moves;
    switch(type) {
        case 'p':
            moves.push_back(Move(coord, 
                    Coord(coord.getFile(), coord.getRank() + (white ? 1 : -1))));
            moves.push_back(Move(coord, 
                    Coord(coord.getFile() - 1, coord.getRank() + (white ? 1 : -1))));
            moves.push_back(Move(coord, 
                    Coord(coord.getFile() + 1, coord.getRank() + (white ? 1 : -1))));
            if(coord.getRank() == (white ? 2 : 7)) {
                moves.push_back(Move(coord, 
                            Coord(coord.getFile(), coord.getRank() + (white ? 2 : -2))));
            }
            if(coord.getRank() == (white ? 7 : 2))
                for(int i = 0, e = moves.size(); i < e; ++i) {
                    moves.push_back(Move(moves[0].getStart(), moves[0].getEnd(), 'n'));
                    moves.push_back(Move(moves[0].getStart(), moves[0].getEnd(), 'b'));
                    moves.push_back(Move(moves[0].getStart(), moves[0].getEnd(), 'r'));
                    moves.push_back(Move(moves[0].getStart(), moves[0].getEnd(), 'q'));
                    moves.erase(moves.begin());
                }
            break;
        case 'n':
            moves.push_back(Move(coord,
                        Coord(coord.getFile() + 2, coord.getRank() + 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() - 2, coord.getRank() + 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() + 2, coord.getRank() - 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() - 2, coord.getRank() - 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() + 1, coord.getRank() + 2)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() - 1, coord.getRank() + 2)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() + 1, coord.getRank() - 2)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() - 1, coord.getRank() - 2)));
            break;
        case 'b':
            for(int i = 1; i <= 7; ++i) {
                moves.push_back(Move(coord,
                            Coord(coord.getFile() + i, coord.getRank() + i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() + i, coord.getRank() - i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() - i, coord.getRank() + i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() - i, coord.getRank() - i)));
            }
            break;
        case 'r':
            for(int i = 1; i <= 7; ++i) {
                moves.push_back(Move(coord,
                            Coord(coord.getFile() + i, coord.getRank())));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() - i, coord.getRank())));
                moves.push_back(Move(coord,
                            Coord(coord.getFile(), coord.getRank() + i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile(), coord.getRank() - i)));
            }
            break;
        case 'q':
            for(int i = 1; i <= 7; ++i) {
                moves.push_back(Move(coord,
                            Coord(coord.getFile() + i, coord.getRank() + i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() + i, coord.getRank() - i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() - i, coord.getRank() + i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() - i, coord.getRank() - i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() + i, coord.getRank())));
                moves.push_back(Move(coord,
                            Coord(coord.getFile() - i, coord.getRank())));
                moves.push_back(Move(coord,
                            Coord(coord.getFile(), coord.getRank() + i)));
                moves.push_back(Move(coord,
                            Coord(coord.getFile(), coord.getRank() - i)));
            }
            break;
        case 'k':
            moves.push_back(Move(coord,
                        Coord(coord.getFile() + 1, coord.getRank() + 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() + 1, coord.getRank() - 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() - 1, coord.getRank() + 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() - 1, coord.getRank() - 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() + 1, coord.getRank())));
            moves.push_back(Move(coord,
                        Coord(coord.getFile() - 1, coord.getRank())));
            moves.push_back(Move(coord,
                        Coord(coord.getFile(), coord.getRank() + 1)));
            moves.push_back(Move(coord,
                        Coord(coord.getFile(), coord.getRank() - 1)));
            break;
    }
    for(int i = 0; i < moves.size(); ++i)
        if(moves[i].getEnd().outOfBounds())
            moves.erase(moves.begin() + i--);
    return moves;
}
