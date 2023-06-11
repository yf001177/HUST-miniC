; ModuleID = 'test'
source_filename = "test"

declare i32 @putchar(i32)

declare i32 @getchar()

define i32 @main() {
entry:
  %b = alloca i32
  store i32 0, i32* %b
  %a = alloca i32
  store i32 0, i32* %a
  %c = alloca i32
  store i32 0, i32* %c
  %d = alloca i32
  store i32 0, i32* %d
  store i32 0, i32* %b
  store i32 2, i32* %c
  %b1 = load i32, i32* %b
  %c2 = load i32, i32* %c
  %add = add i32 %b1, %c2
  store i32 %add, i32* %a
  br label %ret

ret:                                              ; preds = %entry
  %a3 = load i32, i32* %a
  ret i32 %a3
}
