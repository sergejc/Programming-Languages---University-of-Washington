# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.
#require_relative './hw6provided'

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.double_rotate})
    @root.bind('c', proc {@board.cheat})
  end

end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  Cheat_Pieces = [[[0, 0]]]

  All_My_Pieces = All_Pieces + [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]]),
      [[[-1, 0], [-2, 0], [0, 0], [1, 0], [2, 0]],
       [[0, -1], [0, -2], [0, 0], [0, 1], [0, 2]]],
      rotations([[0, 0], [0, 1], [1, 0]])]

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Pieces, board)
  end

  # your enhancements here

end

class MyBoard < Board
  # your enhancements here
  Cheat_Score = 100

  def initialize (game)
    super
    @cheat = false
    @current_block = MyPiece.next_piece(self)
  end

  def next_piece
    super
    if @cheat
      @current_block = MyPiece.cheat_piece(self)
      @cheat = false
    end
  end

  def double_rotate
    rotate_clockwise
    rotate_clockwise
  end

  def next_cheat_piece
    @current_block = MyPiece.cheat_piece(self)
    @current_pos = nil
  end

  def cheat
     if (@score >= Cheat_Score && @cheat == false)
       @score = self.score - Cheat_Score
       @cheat = true
     end
  end
end

