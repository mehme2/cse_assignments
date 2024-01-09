#include "board.h"
#include <stdexcept>

Board::Board(const std::string & s) : 
    _whiteTurn(true), 
    previousMove(Coord('a', 1), Coord('a', 1)) {
    int rank = 8;
    char file = 'a';
    char type;
    bool white;
    for(char c : s) {
        white = false;
        switch(c) {
            case '\n':
                --rank;
                file = 'a';
                break;
            case 'R':
            case 'K':
            case 'B':
            case 'P':
            case 'N':
            case 'Q':
                white = true;
            case 'r':
            case 'k':
            case 'b':
            case 'p':
            case 'n':
            case 'q':
                type = white ? c - 'A' + 'a' : c;
                pieces.push_back(Piece(type, Coord(file, rank), white));
            case '.':
                ++file;
                break;
        }
    }
    generatePossibleMoves();
    generateOutcomes();
}

bool Board::whiteTurn() const {
    return _whiteTurn;
}

std::ostream & operator<<(std::ostream & os, const Board & b) {
    std::vector<std::string> ranks(8, std::string(8, '.'));
    for(auto & p : b.pieces) {
        auto coord = p.getCoords();
        ranks[coord.getRank() - 1][coord.getFile() - 'a'] = p.isWhite() ? p.getType() - 'a' + 'A' : p.getType();
    }
    for(int file = 8; file >= 1; --file) {
        os << file << " |";
        for(char c : ranks[file - 1]) {
            os << ' ' << c;
        }
        os << '\n';
    }
    os << "    ---------------\n    a b c d e f g h\n";
    return os;
}

int Board::pieceAt(const Coord & c) const {
    int idx = -1;
    for(int i = 0; i < pieces.size(); ++i)
        if(pieces[i].getCoords() == c)
            idx = i;
    return idx;
}

const std::vector<Move> Board::getPossibleMoves(bool white) const {
    if(white)
        return whiteMoves;
    else
        return blackMoves;
}

const Board Board::simulateMove(const Move & m) const {
    Board copy(*this);
    int ip1 = pieceAt(m.getStart());
    int ip2 = pieceAt(m.getEnd());
    if(ip1 != -1) {
        copy.pieces[ip1].moveTo(m);
        if(ip2 != -1) {
            copy.pieces.erase(copy.pieces.begin() + ip2);
        }
        copy._whiteTurn = !_whiteTurn;
    }
    copy.outcomes.clear();
    copy.generatePossibleMoves();
    copy.previousMove = m;
    return copy;
}

float Board::getScore(bool white) const {
    if(getPossibleMoves(white).empty())
        return -100.0f;
    float score = 0.0f;
    auto possibleMoves = getPossibleMoves(!white);
    if(possibleMoves.empty())
        score = 100.0f;
    else {
        for(const Piece & p : pieces) {
            if(p.getType() != 'k' && p.isWhite() == white) {
                score += p.getPoints();
                for(const Move & m : possibleMoves) {
                    if(pieces[pieceAt(m.getStart())].canAttack(p, m.getEnd())) {
                        score -= p.getPoints() * 0.5f;
                        break;
                    }
                }
            }
        }
    }
    return score;
}

bool Board::check(bool white) const {
    auto possibleMoves = getPossibleMoves(!white);
    int iK;
    for(iK = 0; iK < pieces.size() && 
            ((pieces[iK].getType() != 'k') || (pieces[iK].isWhite() != white)); ++iK);
    for(const Move & m : possibleMoves) {
        if(pieces[pieceAt(m.getStart())].canAttack(pieces[iK], m.getEnd())) {
            return true;
        }
    }
    return false;
}

void Board::generatePossibleMoves() {
    whiteMoves.clear();
    blackMoves.clear();
    for(const Piece & p : pieces) {
        auto iMoves = p.getPossibleMoves();
        for(Move & m : iMoves) {
            int idx = pieceAt(m.getEnd());
            if(idx != -1) {
                if(!p.canAttack(pieces[idx], m.getEnd()))
                    continue;
            }
            else if(p.getType() == 'p' && p.getCoords().getFile() != m.getEnd().getFile())
                continue;
            if(p.getType() != 'n') {
                int dFile = m.getEnd().getFile() > m.getStart().getFile() ? 1 :
                    m.getEnd().getFile() < m.getStart().getFile() ? -1 : 0;
                int dRank = m.getEnd().getRank() > m.getStart().getRank() ? 1 :
                    m.getEnd().getRank() < m.getStart().getRank() ? -1 : 0;
                Coord i(m.getStart().getFile() + dFile, m.getStart().getRank() + dRank);
                for(; i != m.getEnd(); i.setFile(i.getFile() + dFile), i.setRank(i.getRank() + dRank)) {
                    if(pieceAt(i) != -1) {
                        break;
                    }
                }
                if(i != m.getEnd())
                    continue;
            }
            if(p.isWhite())
                whiteMoves.push_back(m);
            else
                blackMoves.push_back(m);
        }
    }
}

void Board::generateOutcomes() {
    outcomes.clear();
    auto & pm = _whiteTurn ? whiteMoves : blackMoves;
    for(int i = 0; i < pm.size(); ++i) {
        outcomes.push_back(simulateMove(pm[i]));
        if(outcomes.back().check(_whiteTurn)) {
            outcomes.erase(outcomes.end() - 1);
            pm.erase(pm.begin() + i);
            --i;
        }
    }
}

const Move Board::suggest(const int depth) const {
    int iBest;
    float best = -1000.0f;
    int nChecked = 0;
    for(int i = 0; i < outcomes.size(); ++i) {
        std::vector<Board> tree;
        tree.push_back(outcomes[i]);
        for(int j = 1; j < depth; ++j) {
            int size = tree.size();
            for(int k = 0; k < size; ++k) {
                tree[0].generateOutcomes();
                if(!tree[0].outcomes.empty()) {
                    if(tree[0]._whiteTurn == _whiteTurn) {
                        for(const Board & o : tree[0].outcomes)
                            tree.push_back(o);
                    }
                    else {
                        int iWorst;
                        int worstDiff = 1000.0f;
                        for(int k = 0; k < tree[0].outcomes.size(); ++k) {
                            float diff = tree[0].outcomes[k].getScore(_whiteTurn) - tree[0].outcomes[k].getScore(!_whiteTurn);
                            if(diff < worstDiff) {
                                iWorst = k;
                                worstDiff = diff;
                            }
                        }
                        tree.push_back(tree[0].outcomes[iWorst]);
                    }
                }
                else if (tree[0]._whiteTurn != _whiteTurn) {
                    float diff = 1000.0f / j;
                    if(diff > best) {
                        iBest = i;
                        best = diff;
                    }
                }
                tree.erase(tree.begin());
            }
        }
        nChecked += tree.size();
        for(int j = 0; j < tree.size(); ++j) {
            float diff = tree[j].getScore(_whiteTurn) - tree[j].getScore(!_whiteTurn);
            if(diff > best) {
                iBest = i;
                best = diff;
            }
        }
    }
    return outcomes[iBest].previousMove;
}

const Board Board::useMove(const Move & m) const {
    for(const Board & b : outcomes)
        if(b.previousMove == m) {
            Board ret = b;
            ret.generateOutcomes();
            return ret;
        }
    return *this;
}

bool Board::checkMate() const {
    return outcomes.empty();
}

const Move Board::getPreviousMove() const {
    return previousMove;
}
