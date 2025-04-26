/*
Makruk++. Makruk engine in c++
Copyright (C) 2025 Toni Helminen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

export module makrukplusplus;

import <iostream>;
import <array>;
import <vector>;
import <random>;
import <string>;
import <algorithm>;
import <utility>;
import <format>;
import <optional>;
import <stacktrace>;
import <span>;
import <syncstream>;
import <mdspan>;
import <generator>;
import <expected>;
import <functional>;

export namespace MakrukPlusPlus {

// Strongly typed enums with custom formatters
enum class PieceType : uint8_t {
    EMPTY = 0,
    KHON,    // Thai pawn
    MA,      // Horse (knight)
    RUA,     // Rook
    KHUN,    // King
    MET,     // Counselor
    NAI,     // Ferz
    BIA      // Bishop
};

enum class PieceColor : uint8_t {
    NONE = 0,
    BLACK,
    WHITE
};

// Custom formatter for PieceType
template <>
struct std::formatter<PieceType> : std::formatter<char> {
    auto format(PieceType type, auto& ctx) const {
        char c = '.';
        switch (type) {
            case PieceType::KHON: c = 'P'; break;
            case PieceType::MA: c = 'N'; break;
            case PieceType::RUA: c = 'R'; break;
            case PieceType::KHUN: c = 'K'; break;
            case PieceType::MET: c = 'M'; break;
            case PieceType::NAI: c = 'F'; break;
            case PieceType::BIA: c = 'B'; break;
            default: break;
        }
        return std::formatter<char>::format(c, ctx);
    }
};

// Custom formatter for PieceColor
template <>
struct std::formatter<PieceColor> : std::formatter<char> {
    auto format(PieceColor color, auto& ctx) const {
        char c = ' ';
        switch (color) {
            case PieceColor::WHITE: c = 'w'; break;
            case PieceColor::BLACK: c = 'b'; break;
            default: break;
        }
        return std::formatter<char>::format(c, ctx);
    }
};

struct Piece {
    PieceType type;
    PieceColor color;

    constexpr Piece() noexcept : type(PieceType::EMPTY), color(PieceColor::NONE) {}
    constexpr Piece(PieceType t, PieceColor c) noexcept : type(t), color(c) {}

    auto operator<=>(const Piece&) const = default;

    explicit operator bool() const noexcept { return type != PieceType::EMPTY; }
};

// Custom formatter for Piece
template <>
struct std::formatter<Piece> {
    constexpr auto parse(auto& ctx) { return ctx.begin(); }

    auto format(const Piece& p, auto& ctx) const {
        if (!p) return std::format_to(ctx.out(), ".");

        char c = std::format("{}", p.type).front();
        if (p.color == PieceColor::BLACK) {
            c = std::tolower(c);
        }
        return std::format_to(ctx.out(), "{}", c);
    }
};

using BoardStorage = std::array<std::array<Piece, 8>, 8>;
using BoardView = std::mdspan<Piece, std::extents<size_t, 8, 8>>;

struct Move {
    std::array<size_t, 2> from;
    std::array<size_t, 2> to;

    auto operator<=>(const Move&) const = default;
};

// Custom formatter for Move
template <>
struct std::formatter<Move> {
    constexpr auto parse(auto& ctx) { return ctx.begin(); }

    auto format(const Move& m, auto& ctx) const {
        return std::format_to(ctx.out(), "{}{} to {}{}",
            char('a' + m.from[1]), 8 - m.from[0],
            char('a' + m.to[1]), 8 - m.to[0]);
    }
};

enum class MoveError {
    InvalidPosition,
    WrongColor,
    IllegalMove,
    KingInCheck,
    Unknown
};

template <typename T>
using Expected = std::expected<T, std::pair<MoveError, std::stacktrace>>;

// Concept for move generators
template <typename G>
concept MoveGenerator = requires(G g) {
    { g.generate() } -> std::same_as<std::generator<Move>>;
};

class MakrukEngine {
private:
    BoardStorage board_storage;
    BoardView board;
    PieceColor current_player;
    std::mt19937_64 rng;
    std::osyncstream sync_out{std::cout};

    void initialize_board() noexcept {
        // Clear board
        for (auto& row : board_storage) {
            row.fill(Piece{});
        }

        // Set up black pieces (top)
        board_storage[0] = {{
            {PieceType::RUA, PieceColor::BLACK},
            {PieceType::MA, PieceColor::BLACK},
            {PieceType::NAI, PieceColor::BLACK},
            {PieceType::KHUN, PieceColor::BLACK},
            {PieceType::MET, PieceColor::BLACK},
            {PieceType::NAI, PieceColor::BLACK},
            {PieceType::MA, PieceColor::BLACK},
            {PieceType::RUA, PieceColor::BLACK}
        }};

        std::ranges::fill(board_storage[1], Piece{PieceType::KHON, PieceColor::BLACK});

        // Set up white pieces (bottom)
        board_storage[7] = {{
            {PieceType::RUA, PieceColor::WHITE},
            {PieceType::MA, PieceColor::WHITE},
            {PieceType::NAI, PieceColor::WHITE},
            {PieceType::KHUN, PieceColor::WHITE},
            {PieceType::MET, PieceColor::WHITE},
            {PieceType::NAI, PieceColor::WHITE},
            {PieceType::MA, PieceColor::WHITE},
            {PieceType::RUA, PieceColor::WHITE}
        }};

        std::ranges::fill(board_storage[6], Piece{PieceType::KHON, PieceColor::WHITE});

        // Set up BIA (bishops)
        board_storage[2][1] = {PieceType::BIA, PieceColor::BLACK};
        board_storage[2][6] = {PieceType::BIA, PieceColor::BLACK};
        board_storage[5][1] = {PieceType::BIA, PieceColor::WHITE};
        board_storage[5][6] = {PieceType::BIA, PieceColor::WHITE};

        board = BoardView{board_storage.data()->data(), 8, 8};
    }

    constexpr bool is_valid_position(size_t x, size_t y) const noexcept {
        return x < board.extent(0) && y < board.extent(1);
    }

    std::generator<Move> generate_khon_moves(size_t x, size_t y) const {
        const Piece& piece = board[x, y];
        const int forward = (piece.color == PieceColor::WHITE) ? -1 : 1;

        // Forward move
        if (is_valid_position(x + forward, y) {
            const Piece& target = board[x + forward, y];
            if (!target) {
                co_yield Move{{x, y}, {x + forward, y}};
            }
        }

        // Captures (diagonal forward)
        for (const int dx : {-1, 1}) {
            const size_t nx = x + forward;
            const size_t ny = y + dx;
            if (is_valid_position(nx, ny)) {
                const Piece& target = board[nx, ny];
                if (target && target.color != piece.color) {
                    co_yield Move{{x, y}, {nx, ny}};
                }
            }
        }
    }

    std::generator<Move> generate_ma_moves(size_t x, size_t y) const {
        static constexpr std::array<std::pair<int, int>, 8> knight_moves = {{
            {2, 1}, {2, -1}, {-2, 1}, {-2, -1},
            {1, 2}, {1, -2}, {-1, 2}, {-1, -2}
        }};

        const Piece& piece = board[x, y];

        for (const auto& [dx, dy] : knight_moves) {
            const size_t nx = x + dx;
            const size_t ny = y + dy;
            if (is_valid_position(nx, ny)) {
                const Piece& target = board[nx, ny];
                if (!target || target.color != piece.color) {
                    co_yield Move{{x, y}, {nx, ny}};
                }
            }
        }
    }

    std::generator<Move> generate_sliding_moves(size_t x, size_t y, std::span<const std::pair<int, int>> directions) const {
        const Piece& piece = board[x, y];

        for (const auto& [dx, dy] : directions) {
            for (size_t step = 1; step < 8; ++step) {
                const size_t nx = x + dx * step;
                const size_t ny = y + dy * step;
                if (!is_valid_position(nx, ny)) break;

                const Piece& target = board[nx, ny];
                if (target && target.color == piece.color) break;

                co_yield Move{{x, y}, {nx, ny}};
                if (target) break;
            }
        }
    }

    std::generator<Move> generate_rua_moves(size_t x, size_t y) const {
        static constexpr std::array<std::pair<int, int>, 4> rook_directions = {{
            {1, 0}, {-1, 0}, {0, 1}, {0, -1}
        }};

        return generate_sliding_moves(x, y, rook_directions);
    }

    std::generator<Move> generate_bia_moves(size_t x, size_t y) const {
        static constexpr std::array<std::pair<int, int>, 4> bishop_directions = {{
            {1, 1}, {1, -1}, {-1, 1}, {-1, -1}
        }};

        return generate_sliding_moves(x, y, bishop_directions);
    }

    std::generator<Move> generate_khun_moves(size_t x, size_t y) const {
        const Piece& piece = board[x, y];

        for (int dx = -1; dx <= 1; ++dx) {
            for (int dy = -1; dy <= 1; ++dy) {
                if (dx == 0 && dy == 0) continue;

                const size_t nx = x + dx;
                const size_t ny = y + dy;
                if (is_valid_position(nx, ny)) {
                    const Piece& target = board[nx, ny];
                    if (!target || target.color != piece.color) {
                        co_yield Move{{x, y}, {nx, ny}};
                    }
                }
            }
        }
    }

    std::generator<Move> generate_met_or_nai_moves(size_t x, size_t y) const {
        const Piece& piece = board[x, y];

        for (int dx = -1; dx <= 1; ++dx) {
            for (int dy = -1; dy <= 1; ++dy) {
                if (dx == 0 || dy == 0) continue; // Diagonal only

                const size_t nx = x + dx;
                const size_t ny = y + dy;
                if (is_valid_position(nx, ny)) {
                    const Piece& target = board[nx, ny];
                    if (!target || target.color != piece.color) {
                        co_yield Move{{x, y}, {nx, ny}};
                    }
                }
            }
        }
    }

    std::generator<Move> generate_piece_moves(size_t x, size_t y) const {
        const Piece& piece = board[x, y];
        if (piece.color != current_player) co_return;

        switch (piece.type) {
            case PieceType::KHON:
                co_yield generate_khon_moves(x, y);
                break;
            case PieceType::MA:
                co_yield generate_ma_moves(x, y);
                break;
            case PieceType::RUA:
                co_yield generate_rua_moves(x, y);
                break;
            case PieceType::KHUN:
                co_yield generate_khun_moves(x, y);
                break;
            case PieceType::MET:
            case PieceType::NAI:
                co_yield generate_met_or_nai_moves(x, y);
                break;
            case PieceType::BIA:
                co_yield generate_bia_moves(x, y);
                break;
            default:
                break;
        }
    }

    std::vector<Move> collect_all_moves() const {
        std::vector<Move> moves;

        for (size_t x = 0; x < board.extent(0); ++x) {
            for (size_t y = 0; y < board.extent(1); ++y) {
                for (Move move : generate_piece_moves(x, y)) {
                    moves.push_back(move);
                }
            }
        }

        return moves;
    }

    Expected<void> validate_move(const Move& move) const {
        if (!is_valid_position(move.from[0], move.from[1]) ||
            !is_valid_position(move.to[0], move.to[1])) {
            return std::unexpected{
                std::make_pair(MoveError::InvalidPosition, std::stacktrace::current())
            };
        }

        const Piece& piece = board[move.from[0], move.from[1]];
        if (piece.color != current_player) {
            return std::unexpected{
                std::make_pair(MoveError::WrongColor, std::stacktrace::current())
            };
        }

        // TODO: Add more validation (check, etc.)
        return {};
    }

    Expected<void> make_move(const Move& move) {
        if (auto validation = validate_move(move); !validation) {
            return validation;
        }

        Piece& piece = board[move.from[0], move.from[1]];
        Piece& target = board[move.to[0], move.to[1]];

        // Execute move
        target = piece;
        piece = Piece{};

        current_player = (current_player == PieceColor::WHITE) ?
            PieceColor::BLACK : PieceColor::WHITE;

        return {};
    }

    bool is_game_over() const {
        bool white_khun = false;
        bool black_khun = false;

        for (size_t x = 0; x < board.extent(0); ++x) {
            for (size_t y = 0; y < board.extent(1); ++y) {
                const Piece& piece = board[x, y];
                if (piece.type == PieceType::KHUN) {
                    if (piece.color == PieceColor::WHITE) white_khun = true;
                    if (piece.color == PieceColor::BLACK) black_khun = true;
                }
            }
        }

        return !white_khun || !black_khun;
    }

    void print_board() const {
        sync_out << "\n  a b c d e f g h\n";

        for (size_t x = 0; x < board.extent(0); ++x) {
            sync_out << 8 - x << " ";
            for (size_t y = 0; y < board.extent(1); ++y) {
                sync_out << std::format("{} ", board[x, y]);
            }
            sync_out << 8 - x << '\n';
        }

        sync_out << "  a b c d e f g h\n" << std::flush;
    }

public:
    MakrukEngine() :
        board{board_storage.data()->data(), 8, 8},
        current_player{PieceColor::WHITE},
        rng{std::random_device{}()}
    {
        initialize_board();
    }

    void run() {
        sync_out << "Makruk Chess Engine (C++23)\n";

        while (!is_game_over()) {
            print_board();

            auto moves = collect_all_moves();
            if (moves.empty()) {
                sync_out << std::format("{} has no legal moves.\n",
                    current_player == PieceColor::WHITE ? "White" : "Black") << std::flush;
                break;
            }

            // Select random move
            std::uniform_int_distribution<size_t> dist(0, moves.size() - 1);
            const Move move = moves[dist(rng)];

            sync_out << std::format("{} moves {}\n",
                current_player == PieceColor::WHITE ? "White" : "Black", move) << std::flush;

            if (auto result = make_move(move); !result) {
                const auto& [error, trace] = result.error();
                sync_out << std::format("Move error: {} at:\n{}\n",
                    static_cast<int>(error), to_string(trace)) << std::flush;
            }
        }

        sync_out << "Game over!\n";
        print_board();
    }
};

} // namespace Makruk
