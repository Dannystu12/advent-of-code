use super::bingo_board::{has_bingo, BingoBoard, BingoRow};
pub struct BingoGame {
    numbers: Vec<u8>,
    boards: Vec<BingoBoard>,
    winner: Option<BingoBoard>
}

impl BingoGame {
    pub fn new(boards: Vec<BingoBoard>) -> BingoGame {
        BingoGame {
            numbers: vec![],
            boards,
            winner:  None as Option<BingoBoard>
        }
    }

    pub fn get_score(&mut self) -> Option<i32>{
        if self.winner.is_none() {
            return None;
        }
        
        let mut score: i32 = 0;
        for row in self.winner.unwrap().iter() {
            for x in row {
                if !self.numbers.contains(x) {
                    score += *x as i32;
                }
            }
        }

        Some(score * *self.numbers.iter().last().unwrap() as i32)
    }

    pub fn draw_number(&mut self, num: u8) -> Option<BingoBoard> {
        
        if self.winner.is_some() {
            return self.winner;
        }

        self.numbers.push(num);

        // Check if there is a winner
        for board in self.boards.iter() {
            let bingo = has_bingo(board, &self.numbers);
            if bingo.is_some() {
                self.winner = Some(*board);
                return Some(*board);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static BINGO_BOARD_1: BingoBoard = [
        [22, 13, 17, 11, 0],
        [8, 2, 23, 4, 24],
        [21, 9, 14, 16, 7],
        [6, 10, 3, 18, 5],
        [1, 12, 20, 15, 19],
    ];

    static BINGO_BOARD_2: BingoBoard = [
        [3, 15, 0, 2, 22],
        [9, 18, 13, 17, 5],
        [19, 8, 7, 25, 23],
        [20, 11, 10, 24, 4],
        [14, 21, 16, 12, 6],
    ];

    static BINGO_BOARD_3: BingoBoard = [
        [14, 21, 17, 24, 4],
        [10, 16, 15, 9, 19],
        [18, 8, 23, 26, 20],
        [22, 11, 13, 6, 5],
        [2, 0, 12, 3, 7],
    ];

    

    mod get_score_tests {
        use super::*;

        #[test]
        fn sample_report_test() {
            let mut game = BingoGame::new(vec![BINGO_BOARD_3]);
            for i in [7,4,9,5,11,17,23,2,0,14,21,24] {
                game.draw_number(i);
            }
            let ans = game.get_score();
            assert_eq!(ans, Some(4512));
        }
    }

    mod draw_number_tests {
        use super::*;

        #[test]
        fn sample_report_test() {
            let mut game = BingoGame::new(vec![BINGO_BOARD_1, BINGO_BOARD_2, BINGO_BOARD_3]);
            let mut ans = None;
            for i in [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24] {
                ans = game.draw_number(i);
            }

            assert_eq!(ans, Some(BINGO_BOARD_3));
            assert_eq!(game.winner, Some(BINGO_BOARD_3));

        }
    }
}
