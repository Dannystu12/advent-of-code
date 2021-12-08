
use super::bingo_board::BingoBoard;
pub struct BingoGame {
    numbers: Vec<u8>,
    boards: Vec<BingoBoard>,
}

impl BingoGame {
    pub fn new(boards: Vec<BingoBoard>) -> BingoGame {
        BingoGame {
            numbers: vec![],
            boards,
        }
    }

    pub fn draw_number(&mut self, num: u8) -> Option<BingoGame> {
        todo!();
        // Append number to numbers called
        // Check if there is a winner
    }
}

