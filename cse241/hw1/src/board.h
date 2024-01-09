#include <string>
#include <iostream>
#include "piece.h"

class Board {
public:
    Board(const std::string & b = 
            "rnbqkbnr\n"
            "pppppppp\n"
            "........\n"
            "........\n"
            "........\n"
            "........\n"
            "PPPPPPPP\n"
            "RNBQKBNR\n"
            );
    bool whiteTurn() const;
    float getScore(bool white) const;
    bool check(bool white) const;
    bool checkMate() const;
    const Move suggest(int depth) const;
    const Move getPreviousMove() const;
    const Board useMove(const Move & m) const;
    const std::vector<Move> getPossibleMoves(bool white) const;
    friend std::ostream & operator<<(std::ostream & os, const Board & b);
private:
    const Board simulateMove(const Move & m) const;
    int pieceAt(const Coord & c) const;
    void generatePossibleMoves();
    void generateOutcomes();
    bool _whiteTurn;
    Move previousMove;
    std::vector<Piece> pieces;
    std::vector<Move> whiteMoves;
    std::vector<Move> blackMoves;
    std::vector<Board> outcomes;
};
