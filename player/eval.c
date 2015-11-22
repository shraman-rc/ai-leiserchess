// Copyright (c) 2015 MIT License by 6.172 Staff

#include "./eval.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "./tbassert.h"

// -----------------------------------------------------------------------------
// Evaluation
// -----------------------------------------------------------------------------

typedef int32_t ev_score_t;  // Static evaluator uses "hi res" values

int RANDOMIZE;

int PCENTRAL;
int HATTACK;
int PBETWEEN;
int PCENTRAL;
int KFACE;
int KAGGRESSIVE;
int MOBILITY;
int PAWNPIN;

size_t squares[100] = { 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
                        83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
                        115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140,
                        147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172,
                        179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204 };

// Heuristics for static evaluation - described in the google doc
// mentioned in the handout.


// PCENTRAL heuristic: Bonus for Pawn near center of board
ev_score_t pcentral(fil_t f, rnk_t r) {
  double df = BOARD_WIDTH/2 - f - 1;
  if (df < 0)  df = f - BOARD_WIDTH/2;
  double dr = BOARD_WIDTH/2 - r - 1;
  if (dr < 0) dr = r - BOARD_WIDTH/2;
  double bonus = 1 - sqrt(df * df + dr * dr) / (BOARD_WIDTH / sqrt(2));
  return PCENTRAL * bonus;
}


// returns true if c lies on or between a and b, which are not ordered
bool between(int c, int a, int b) {
  bool x = ((c >= a) && (c <= b)) || ((c <= a) && (c >= b));
  return x;
}

// PBETWEEN heuristic: Bonus for Pawn at (f, r) in rectangle defined by Kings at the corners
ev_score_t pbetween(position_t *p, fil_t f, rnk_t r) {
  square_t white_kloc = p->kloc[WHITE];
  square_t black_kloc = p->kloc[BLACK];
  bool is_between =
      between(f, fil_of(white_kloc), fil_of(black_kloc)) &&
      between(r, rnk_of(white_kloc), rnk_of(black_kloc));

  return is_between ? PBETWEEN : 0;
}


// KFACE heuristic: bonus (or penalty) for King facing toward the other King
ev_score_t kface(position_t *p, fil_t f, rnk_t r) {
  square_t sq = square_of(f, r);
  piece_t x = p->board[sq];
  color_t c = color_of(x);
  square_t opp_sq = p->kloc[opp_color(c)];
  int delta_fil = fil_of(opp_sq) - f;
  int delta_rnk = rnk_of(opp_sq) - r;
  int bonus;

  switch (ori_of(x)) {
    case NN:
      bonus = delta_rnk;
      break;

    case EE:
      bonus = delta_fil;
      break;

    case SS:
      bonus = -delta_rnk;
      break;

    case WW:
      bonus = -delta_fil;
      break;

    default:
      bonus = 0;
      tbassert(false, "Illegal King orientation.\n");
  }

  return (bonus * KFACE) / (abs(delta_rnk) + abs(delta_fil));
}

// KAGGRESSIVE heuristic: bonus for King with more space to back
ev_score_t kaggressive(position_t *p, fil_t f, rnk_t r) {
  square_t sq = square_of(f, r);
  piece_t x = p->board[sq];
  color_t c = color_of(x);
  tbassert(ptype_of(x) == KING, "ptype_of(x) = %d\n", ptype_of(x));

  square_t opp_sq = p->kloc[opp_color(c)];
  fil_t of = fil_of(opp_sq);
  rnk_t _or = (rnk_t) rnk_of(opp_sq);

  int delta_fil = of - f;
  int delta_rnk = _or - r;

  int bonus = 0;

  if (delta_fil >= 0 && delta_rnk >= 0) {
    bonus = (f + 1) * (r + 1);
  } else if (delta_fil <= 0 && delta_rnk >= 0) {
    bonus = (BOARD_WIDTH - f) * (r + 1);
  } else if (delta_fil <= 0 && delta_rnk <= 0) {
    bonus = (BOARD_WIDTH - f) * (BOARD_WIDTH - r);
  } else if (delta_fil >= 0 && delta_rnk <= 0) {
    bonus = (f + 1) * (BOARD_WIDTH - r);
  }

  return (KAGGRESSIVE * bonus) / (BOARD_WIDTH * BOARD_WIDTH);
}

// Marks the path of the laser until it hits a piece or goes off the board.
//
// p : current board state
// laser_map : end result will be stored here. Every square on the
//             path of the laser is marked with mark_mask
// c : color of king shooting laser
// mark_mask: what each square is marked with
// NOTE: most of this code is duplicated in mobility,
// calculate_pawnpin_scores/laser_path_count_pawns, and h_squares_attackable
void mark_laser_path(position_t *p, char *laser_map, color_t c,
                     char mark_mask) {
  position_t np = *p;

  // Fire laser, recording in laser_map
  square_t sq = np.kloc[c];
  int bdir = ori_of(np.board[sq]);

  tbassert(ptype_of(np.board[sq]) == KING,
           "ptype: %d\n", ptype_of(np.board[sq]));
  laser_map[sq] |= mark_mask;

  while (true) {
    sq += beam_of(bdir);
    laser_map[sq] |= mark_mask;
    tbassert(sq < ARR_SIZE && sq >= 0, "sq: %d\n", sq);

    switch (ptype_of(p->board[sq])) {
      case EMPTY:  // empty square
        break;
      case PAWN:  // Pawn
        bdir = reflect_of(bdir, ori_of(p->board[sq]));
        if (bdir < 0) {  // Hit back of Pawn
          return;
        }
        break;
      case KING:  // King
        return;  // sorry, game over my friend!
        break;
      case INVALID:  // Ran off edge of board
        return;
        break;
      default:  // Shouldna happen, man!
        tbassert(false, "Not cool, man.  Not cool.\n");
        break;
    }
  }
}

// TODO: cache values for o_king_loc [nah not significant]
// Harmonic-ish distance: 1/(|dx|+1) + 1/(|dy|+1)
float h_dist(square_t a, square_t b) {
  //  printf("a = %d, FIL(a) = %d, RNK(a) = %d\n", a, FIL(a), RNK(a));
  //  printf("b = %d, FIL(b) = %d, RNK(b) = %d\n", b, FIL(b), RNK(b));
  int delta_fil = abs(fil_of(a) - fil_of(b));
  int delta_rnk = abs(rnk_of(a) - rnk_of(b));
  float x = (1.0 / (delta_fil + 1)) + (1.0 / (delta_rnk + 1));
  //  printf("max_dist = %d\n\n", x);
  return x;
}

// PAWNPIN Heuristic --- is a pawn immobilized by the enemy laser.
// H_SQUARES_ATTACKABLE heuristic: for shooting the enemy king.
// MOBILITY heuristic: safe squares around king of color color.
// c is the color of the king shooting the laser
void compute_all_laser_path_heuristics(position_t* p, color_t c, ev_score_t* scores, int o_pawns, bool verbose) {
  color_t opp_c = opp_color(c);

  int o_pinned_pawns = 0;  // number of pinned pawns of color opp_c
  float h_attackable = 0;  // for king of color c

  // mark true when hit by laser
  // only used by mobility computation
  bool laser_map[ARR_SIZE] = {false};

  position_t np = *p;
  square_t sq = np.kloc[c];
  int bdir = ori_of(np.board[sq]);

  tbassert(ptype_of(np.board[sq]) == KING,
           "ptype: %d\n", ptype_of(np.board[sq]));
  square_t o_king_sq = p->kloc[opp_c];
  tbassert(ptype_of(p->board[o_king_sq]) == KING,
           "ptype: %d\n", ptype_of(p->board[o_king_sq]));
  tbassert(color_of(p->board[o_king_sq]) == color,
           "color: %d\n", color_of(p->board[o_king_sq]));

  laser_map[sq] = true;
  h_attackable += h_dist(sq, o_king_sq);

  bool loop = true;
  while (loop) {
    sq += beam_of(bdir);
    tbassert(sq < ARR_SIZE && sq >= 0, "sq: %d\n", sq);

    laser_map[sq] = true;

    piece_t piece = p->board[sq];
    switch (ptype_of(piece)) {
      case EMPTY:  // empty square
        h_attackable += h_dist(sq, o_king_sq);
        break;
      case PAWN:  // Pawn
        h_attackable += h_dist(sq, o_king_sq);

        if (color_of(piece) == opp_c) {
          o_pinned_pawns++;
        }

        bdir = reflect_of(bdir, ori_of(piece));
        if (bdir < 0) {  // Hit back of Pawn
          loop = false;
        }
        break;
      case KING:  // King
        h_attackable += h_dist(sq, o_king_sq);
        loop = false;  // sorry, game over my friend!
        break;
      case INVALID:  // Ran off edge of board
        loop = false;
        break;
      default:  // Shouldna happen, man!
        tbassert(false, "Not cool, man.  Not cool.\n");
        break;
    }
  }

  int o_mobility = 0;  // mobility of king of color opp_c
  if (laser_map[o_king_sq] == false) {
    o_mobility++;
  }
  for (int d = 0; d < 8; ++d) {
    square_t sq = o_king_sq + dir_of(d);
    if (rnk_of(sq) >= 0 && rnk_of(sq) < BOARD_WIDTH && fil_of(sq) >= 0 && fil_of(sq) < BOARD_WIDTH) {
      if (laser_map[sq] == false) {
        o_mobility++;
      }
    }
  }

  o_mobility *= MOBILITY;
  scores[opp_c] += o_mobility;

  if (verbose) {
    if (opp_c == WHITE) {
      printf("MOBILITY bonus %d for White \n", o_mobility);
    } else {
      printf("MOBILITY bonus %d for Black \n", o_mobility);
    }
  }

  h_attackable = (int)h_attackable * HATTACK;
  scores[c] += h_attackable;

  if (verbose) {
    if (c == WHITE) {
      printf("HATTACK bonus %f for White \n", h_attackable);
    } else {
      printf("HATTACK bonus %f for Black \n", h_attackable);
    }
  }

  scores[opp_c] += PAWNPIN * (o_pawns - o_pinned_pawns);
}

// Static evaluation.  Returns score
score_t eval(position_t *p, bool verbose) {
  // seed rand_r with a value of 1, as per
  // http://linux.die.net/man/3/rand_r
  static __thread unsigned int seed = 1;
  // verbose = true: print out components of score
  ev_score_t score[2] = { 0, 0 };
  //  int corner[2][2] = { {INF, INF}, {INF, INF} };
  ev_score_t bonus;
  char buf[MAX_CHARS_IN_MOVE];

  int pawn_counts[2] = {0};

  for (fil_t f = 0; f < BOARD_WIDTH; f++) {
    for (rnk_t r = 0; r < BOARD_WIDTH; r++) {
      square_t sq = square_of(f, r);
      piece_t x = p->board[sq];
      color_t c = color_of(x);
      if (verbose) {
        square_to_str(sq, buf, MAX_CHARS_IN_MOVE);
      }

      switch (ptype_of(x)) {
        case EMPTY:
          break;
        case PAWN:
          pawn_counts[c]++;

          // MATERIAL heuristic: Bonus for each Pawn
          if (verbose) {
            printf("MATERIAL bonus %d for %s Pawn on %s\n", PAWN_EV_VALUE, color_to_str(c), buf);
          }
          // now added once at the end of the loop
          // score[c] += bonus;

          // PBETWEEN heuristic
          bonus = pbetween(p, f, r);
          if (verbose) {
            printf("PBETWEEN bonus %d for %s Pawn on %s\n", bonus, color_to_str(c), buf);
          }
          score[c] += bonus;

          // PCENTRAL heuristic
          bonus = pcentral(f, r);
          if (verbose) {
            printf("PCENTRAL bonus %d for %s Pawn on %s\n", bonus, color_to_str(c), buf);
          }
          score[c] += bonus;
          break;

        case KING:
          // KFACE heuristic
          bonus = kface(p, f, r);
          if (verbose) {
            printf("KFACE bonus %d for %s King on %s\n", bonus,
                   color_to_str(c), buf);
          }
          score[c] += bonus;

          // KAGGRESSIVE heuristic
          bonus = kaggressive(p, f, r);
          if (verbose) {
            printf("KAGGRESSIVE bonus %d for %s King on %s\n", bonus, color_to_str(c), buf);
          }
          score[c] += bonus;
          break;
        case INVALID:
          break;
        default:
          tbassert(false, "Jose says: no way!\n");   // No way, Jose!
      }
    }
  }

  // MATERIAL heuristic
  score[WHITE] += pawn_counts[WHITE] * PAWN_EV_VALUE;
  score[BLACK] += pawn_counts[BLACK] * PAWN_EV_VALUE;

  compute_all_laser_path_heuristics(p, WHITE, score, pawn_counts[BLACK], verbose);
  compute_all_laser_path_heuristics(p, BLACK, score, pawn_counts[WHITE], verbose);

  // score from WHITE point of view
  ev_score_t tot = score[WHITE] - score[BLACK];

  if (RANDOMIZE) {
    ev_score_t  z = rand_r(&seed) % (RANDOMIZE*2+1);
    tot = tot + z - RANDOMIZE;
  }

  if (color_to_move_of(p) == BLACK) {
    tot = -tot;
  }

  return tot / EV_SCORE_RATIO;
}
