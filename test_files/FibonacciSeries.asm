	@256
	D=A
	@SP
	M=D
	@RETURN0
	D=A
	@SP
	AM=M+1
	A=A-1
	M=D
	@LCL
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@ARG
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@THIS
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@THAT
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@SP
	D=M
	@LCL
	M=D
	@5
	D=D-A
	@ARG
	M=D
	@Sys.init
	0;JMP
(RETURN0)
	@1
	D=A
	@ARG
	A=D+M
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@SP
	AM=M-1
	D=M
	@R4
	M=D
	@0
	D=A
	@SP
	AM=M+1
	A=A-1
	M=D
	@0
	D=A
	@THAT
	M=D+M
	@SP
	AM=M-1
	D=M
	@THAT
	A=M
	M=D
	@0
	D=A
	@THAT
	M=M-D
	@1
	D=A
	@SP
	AM=M+1
	A=A-1
	M=D
	@1
	D=A
	@THAT
	M=D+M
	@SP
	AM=M-1
	D=M
	@THAT
	A=M
	M=D
	@1
	D=A
	@THAT
	M=M-D
	@0
	D=A
	@ARG
	A=D+M
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@2
	D=A
	@SP
	AM=M+1
	A=A-1
	M=D
	@SP
	AM=M-1
	D=M
	@SP
	A=M-1
	M=M-D
	@0
	D=A
	@ARG
	M=D+M
	@SP
	AM=M-1
	D=M
	@ARG
	A=M
	M=D
	@0
	D=A
	@ARG
	M=M-D
($MAIN_LOOP_START)
	@0
	D=A
	@ARG
	A=D+M
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@SP
	AM=M-1
	D=M
	@$COMPUTE_ELEMENT
	D;JNE
	@$END_PROGRAM
	0;JMP
($COMPUTE_ELEMENT)
	@0
	D=A
	@THAT
	A=D+M
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@1
	D=A
	@THAT
	A=D+M
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@SP
	AM=M-1
	D=M
	@SP
	A=M-1
	M=D+M
	@2
	D=A
	@THAT
	M=D+M
	@SP
	AM=M-1
	D=M
	@THAT
	A=M
	M=D
	@2
	D=A
	@THAT
	M=M-D
	@R4
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@1
	D=A
	@SP
	AM=M+1
	A=A-1
	M=D
	@SP
	AM=M-1
	D=M
	@SP
	A=M-1
	M=D+M
	@SP
	AM=M-1
	D=M
	@R4
	M=D
	@0
	D=A
	@ARG
	A=D+M
	D=M
	@SP
	AM=M+1
	A=A-1
	M=D
	@1
	D=A
	@SP
	AM=M+1
	A=A-1
	M=D
	@SP
	AM=M-1
	D=M
	@SP
	A=M-1
	M=M-D
	@0
	D=A
	@ARG
	M=D+M
	@SP
	AM=M-1
	D=M
	@ARG
	A=M
	M=D
	@0
	D=A
	@ARG
	M=M-D
	@$MAIN_LOOP_START
	0;JMP
($END_PROGRAM)
(END)
	@END
	0;JMP
