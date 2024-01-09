#include "board.h"
#include <fstream>

int main() {
    std::vector<Board> history;
    Board b;
    std::cout << "\nWelcome to the Chess Game!\n" <<
                 "\nType \"help\" to the list of commands.\n\n" << b;
    while(true) {
        std::string prompt;
        if(b.checkMate()) {
            std::cout << (b.whiteTurn() ? "Black" : "White") << " won!\n";
            break;
        }
        std::cout << "\n[" << 
            (b.whiteTurn() ? "White" : "Black") << "'s Turn]\n\nYour score is: " <<
            b.getScore(b.whiteTurn()) << "\n\nEnter your move: ";
        bool legal = false;
        while(!legal) {
            legal = true;
            std::cin >> prompt;
            if(prompt == "help") {
                std::cout << "\nList of Commands:\n"
                             "\nundo: Undoes the last move. Fails if no move has been made.\n"
                             "\nsave (file path): Saves the current game to the specified file.\n"
                             "\nload (file path): Tries to load a game from the specified file.\n"
                             "\nsuggest: Suggest a move for the current player.\n"
                             "\nAny move with the standard notation: Uses the move and advances to the next turn if the move is legal.\n"
                             "\nE.g.: d2d4, a7a8q (promotes the pawn to a queen)\n";
            }
            else if(prompt == "suggest") {
                Move move(b.suggest(5));
                std::cout << "\nSuggestion is " << move << '\n';
            }
            else if(prompt == "save") {
                std::cin >> prompt;
                std::ofstream out(prompt.c_str());
                if(out.is_open()) {
                    if(history.size() > 0) {
                        for(int i = 1; i < history.size(); ++i)
                            out << history[i].getPreviousMove() << '\n';
                        out << b.getPreviousMove();
                    }
                    out.close();
                }
                else {
                    std::cout << "\nError opening file \"" << prompt <<"\".\n";
                }
            }
            else if(prompt == "load") {
                std::cin >> prompt;
                std::ifstream in(prompt.c_str());
                if(in.is_open()) {
                    history.clear();
                    b = Board();
                    while(!in.eof()) {
                        std::string moveStr;
                        in >> moveStr;
                        Board n = b;
                        if(moveStr.size() >= 4) {
                            Move move(Coord(moveStr[0], moveStr[1] - '0'),
                                Coord(moveStr[2], moveStr[3] - '0'), moveStr.size() > 4 ? prompt[4] : 0);
                            n = n.useMove(move);
                        }
                        if(n.whiteTurn() == b.whiteTurn()) {
                            std::cout << "\nIllegal move found: " << moveStr << "\n";
                        }
                        else {
                            history.push_back(b);
                            b = n;
                        }
                    }
                    std::cout << "\nLoaded " << history.size() << " moves.\n";
                    std::cout << "\n[Updated Board]\n" << b;
                    in.close();
                }
                else {
                    std::cout << "\nError opening file \"" << prompt <<"\".\n";
                }
            }
            else if(prompt == "undo" && !history.empty()) {
                b = history[history.size() - 1];
                history.pop_back();
                std::cout << "\n[Updated Board]\n" << b;
            }
            else if(prompt.size() >= 4) {
                Move move(Coord(prompt[0], prompt[1] - '0'),
                        Coord(prompt[2], prompt[3] - '0'), prompt.size() > 4 ? prompt[4] : 0);
                Board n(b.useMove(move));
                if(n.whiteTurn() == b.whiteTurn())
                    legal = false;
                else {
                    history.push_back(b);
                    b = n;
                    std::cout << "\n[Updated Board]\n" << b;
                }
            }
            else {
                legal = false;
            }
            if(!legal) {
                std::cout << "\nIllegal prompt.\n\nEnter your move: ";
                prompt.clear();
            }
        }
    }
    return 0;
}
