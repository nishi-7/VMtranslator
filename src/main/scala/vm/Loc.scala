package vm


// Tokenの位置を表すクラス
// """file: example.vm
//    line: 10
//    push hoge 0
//         ^^^^
//   error: expected segment, but got illegal token"""
case class Loc(file: String, line: Int, left: Int, len: Int)
