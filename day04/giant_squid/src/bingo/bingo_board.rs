pub type BingoBoard = [BingoRow; 5];
pub type BingoRow = [u8; 5];

pub fn has_bingo(board: &BingoBoard, numbers_called: &Vec<u8>) -> Option<BingoRow> {
    has_horizontal_line(&board, &numbers_called)
        .or(has_vertical_line(&board, &numbers_called))
        //.or(has_diagonal_line(&board, &numbers_called));
}

fn has_horizontal_line(board: &BingoBoard, numbers_called: &Vec<u8>) -> Option<BingoRow> {
    has_winning_row(&board.to_vec(), &numbers_called)
}

fn has_vertical_line(board: &BingoBoard, numbers_called: &Vec<u8>) -> Option<BingoRow> {
    let mut rows_to_check: BingoBoard = [[0; 5]; 5];
    for (i, row) in board.iter().enumerate() {
        for (j, v) in row.iter().enumerate() {
            rows_to_check[j][i] = *v;
        }
    }
    has_winning_row(&rows_to_check.to_vec(), &numbers_called)
}

fn has_diagonal_line(board: &BingoBoard, numbers_called: &Vec<u8>) -> Option<BingoRow> {
    let mut rows_to_check: [BingoRow; 2] = [[0; 5]; 2];
    for (i, row) in board.iter().enumerate() {
        for (j, num) in row.iter().enumerate() {
            let right_idx = row.len() - i - 1;
            if j == i {
                rows_to_check[0][i] = *num;
            }

            if j == right_idx {
                rows_to_check[1][i] = *num;
            }
        }
    }

    has_winning_row(&rows_to_check.to_vec(), &numbers_called)
}

fn has_winning_row(rows_to_check: &Vec<BingoRow>, numbers_called: &Vec<u8>) -> Option<BingoRow> {
    let winning_rows = rows_to_check
        .iter()
        .filter(|x| x.iter().all(|y| numbers_called.contains(y)));

    for wr in winning_rows {
        return Some(*wr);
    }

    return None;
}

#[cfg(test)]
mod tests {
    use super::*;

    static BINGO_BOARD: BingoBoard = [
        [22, 13, 17, 11, 0],
        [8, 2, 23, 4, 24],
        [21, 9, 14, 16, 7],
        [6, 10, 3, 18, 5],
        [1, 12, 20, 15, 19],
    ];

    mod has_horizontal_line_tests {
        use super::*;

        #[test]
        fn test_no_numbers_called() {
            let ans = has_horizontal_line(&BINGO_BOARD, &vec![]);
            assert_eq!(ans, None);
        }

        #[test]
        fn test_no_win() {
            let ans = has_horizontal_line(&BINGO_BOARD, &vec![1, 2, 3, 4, 5, 6]);
            assert_eq!(ans, None);
        }

        #[test]
        fn test_win() {
            let ans = has_horizontal_line(&BINGO_BOARD, &vec![1, 12, 20, 15, 19]);
            assert_eq!(ans, Some([1, 12, 20, 15, 19]));
        }
    }

    mod has_vertical_line_tests {
        use super::*;

        #[test]
        fn test_no_numbers_called() {
            let ans = has_vertical_line(&BINGO_BOARD, &vec![]);
            assert_eq!(ans, None);
        }

        #[test]
        fn test_no_win() {
            let ans = has_vertical_line(&BINGO_BOARD, &vec![1, 2, 3, 4, 5, 6]);
            assert_eq!(ans, None);
        }

        #[test]
        fn test_win() {
            let ans = has_vertical_line(&BINGO_BOARD, &vec![22, 8, 21, 6, 1]);
            assert_eq!(ans, Some([22, 8, 21, 6, 1]));
        }
    }

    mod has_diagonal_line_tests {
        use super::*;

        #[test]
        fn test_no_numbers_called() {
            let ans = has_diagonal_line(&BINGO_BOARD, &vec![]);
            assert_eq!(ans, None);
        }

        #[test]
        fn test_no_win() {
            let ans = has_diagonal_line(&BINGO_BOARD, &vec![1, 2, 3, 4, 5, 6]);
            assert_eq!(ans, None);
        }

        #[test]
        fn test_win_ltr() {
            let ans = has_diagonal_line(&BINGO_BOARD, &vec![22, 2, 14, 18, 19]);
            assert_eq!(ans, Some([22, 2, 14, 18, 19]));
        }

        #[test]
        fn test_win_rtl() {
            let ans = has_diagonal_line(&BINGO_BOARD, &vec![0, 4, 14, 10, 1]);
            assert_eq!(ans, Some([0, 4, 14, 10, 1]));
        }
    }
}
