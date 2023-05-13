DATAS SEGMENT
    string db 'a',20h,120,0,'b',20h,12h,01h,'c',20h,0ach,01h,12 DUP(0)
    speed db 6
    range db 0;整型随机数的上限
    num db 0;生成的随机数
    input db '-';用户输入的字符
    count db 12;下一个待生成的字符的偏移（只需要8位即可，由于string最大100字节）
    score dw 0;正确的次数
    click dw 0;点击次数
    speed_select db "please input a number to choose speed(1 to 9 recomended)$"
    feedbackMessage db "  great!  your accuracy achieved $"
    feedbackMessage2 db ",  more than $"
    continue_select db"input e to exit, c to continue, a to play again: $"
    error_info db "input error!$"
    
DATAS ENDS

STACKS SEGMENT
    db 128 dup(0)
STACKS ENDS

CODES SEGMENT
.386
    ASSUME CS:CODES,DS:DATAS,SS:STACKS
START:
    MOV AX,DATAS
    MOV DS,AX
    MOV AX,0
	MOV ES,AX
    call install
    
initial:
	call clear
    mov dx,offset speed_select
    mov ah,09h
    int 21h
    
    mov ah,00h
    int 16h
   
   	cmp al,30h
   	jb initial
   	cmp al,39h
   	ja initial
   
   
   
    sub al,30h
    mov speed,al
	
loop_main:
	mov cx,4
	cmp byte ptr es:[206H],0
	jnz feedback
	call refresh	
	call sleep
	
	cmp byte ptr es:[206H],0
	jnz feedback
	call refresh	
	call sleep
	
	cmp byte ptr es:[206H],0
	jnz feedback
	call refresh	
	call sleep
	
	cmp byte ptr es:[206H],0
	jnz feedback
	call refresh	
	call sleep
	
	cmp byte ptr es:[206H],0
	jnz feedback
	call refresh	
	call sleep
	
	
	CALL GENERATE
	
	
	loop loop_main
	

	
feedback:
	call clear
	
	mov dx,offset feedbackMessage
    mov ah,09h
    int 21h
    
	mov bx,score
	call BINTODEC
	
	mov dl,'/'
	mov ah,02h
	int 21h
	
	mov bx,click
	call BINTODEC
	
	mov dx,offset feedbackMessage2
    mov ah,09h
    int 21h
    
    call percent;返回值直接在bx内
    call BINTODEC
    
	mov dl,'%'
	mov ah,02h
	int 21h
	
	
	call crlf
	
	mov dx,offset continue_select
    mov ah,09h
    int 21h
	
main_option:

    mov ah,00h
    int 16h
	
	cmp al,'e'
	jz main_ret
	cmp al,'c'
	je main_continue
	cmp al,'a'
	je main_back

	
	call crlf
	
    mov dx,offset error_info
    mov ah,09h
    int 21h
    
	call crlf
    
	mov dx,offset continue_select
    mov ah,09h
    int 21h
	jmp main_option
	
	
main_back:
	mov cx,8
	mov bx,offset string
loop_clearstring:
	mov dword ptr [bx],0
	add bx,4
	loop loop_clearstring
	mov click,0
	mov score,0
main_continue:
	mov ax,0
	mov es,ax
	mov byte ptr es:[206H],0
	jmp initial
	
	
main_ret:

    MOV AH,4CH
    INT 21H
    
crlf proc 
    mov dl,0dh
	mov ah,02h
	int 21h
	
	mov dl,0ah
	mov ah,02h
	int 21h
    ret
crlf endp
    
;清屏
clear proc
	push ax
	MOV AH, 00H  ; 子功能号
	MOV AL, 03H  ; 清屏属性（黑底白字）
	INT 10H
	pop ax
	ret
clear endp




;刷新屏幕
;使用了es，但是有现场保护
refresh proc
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	;清屏
	call clear

	mov ax,0b800h
	mov es,ax
	lea si,string
	
	;循环操作---找到位置/设置字符与属性/更新位置
	mov cx,6
refresh1:
	mov dx,[si]
	cmp dl,61h;;;;;;;;;
	jb refresh_special;如果字符的ascii码小于61h的话，就不显示了，说明是无效字符
	mov di,[si+2]
	cmp di,4000
	ja refresh_special;如果在显存区的偏移大于4000，放弃显示，直接下一轮
	add word ptr [si+2],160;使下次显示时换行
	mov es:[di],dx
refresh_special:
	add si,4
	loop refresh1

refresh_ret:
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
refresh endp





SLEEP PROC
	push ax
	push cx
	push es
	
	XOR cx,cx
	mov cl,speed
loop_outDelay:
	push cx
	mov cx,15000
LOOP_DELAY:
 	
    MOV AH, 01h   ; 设置AH寄存器为01h，表示检查键盘缓冲区
    INT 16h       ; 调用BIOS中断16h，检查键盘缓冲区
    JZ delay_continue ; 如果键盘缓冲区为空，则跳过click自增


	inc click
    MOV AH, 00h   ; 设置AH寄存器为00h，表示读取键盘扫描码
    INT 16h       ; 调用BIOS中断16h，读取键盘扫描码
	
	;将字符的ASCII码存入input调用check接口
	mov input,al
	call check
delay_continue:
	
	cmp byte ptr es:[206h],0
	jnz delay_special
	
  	loop LOOP_DELAY
	pop cx
	loop loop_outDelay
	jmp delay_ret
	
	
delay_special:
	pop cx
	
delay_ret:
	
	pop es
  	pop cx
  	pop ax
  	ret             ; 返回
SLEEP ENDP



;比较与修改函数，前提是input内输入了ASCII码
;参数 byte input ，word score为答对的分数
;只修改了string没有使用es
check proc
	push bx
	push cx
	push dx

	mov dl,input
	lea bx,string
	mov cx,6
check_loop:
	cmp byte ptr [bx],dl
	je check_special
check_continue:
	add bx,4
	loop check_loop
	jmp check_ret
check_special:
	cmp byte ptr [bx+1],20h
	jz check_continue
	mov byte ptr [bx+1],20h;前景色显示为绿色
	inc score
	call buzzer
check_ret:
	pop dx
	pop cx
	pop bx
	ret
check endp
    
    
;使蜂鸣器发出声音
buzzer proc
    push ax
    push cx
    
    mov al, 182             ; 设置方波jk模式
    out 43h, al             ; 将设置写入控制寄存器
    
    mov ax, 1           ; 设置计数器初值
    out 42h, al             ; 将低8位写入计数器
    mov al, ah
    out 42h, al             ; 将高8位写入计数器
    
    in al, 61h              ; 读取键盘控制寄存器
    or al, 3                ; 打开PC喇叭的控制位
    out 61h, al             ; 将设置写入键盘控制寄存器
    
    mov cx, 5000            ; 延时约0.1秒
delay1:
    loop delay1              ; 循环延时
    
    in al, 61h              ; 关闭PC喇叭的控制位
    and al, 11111100b
    out 61h, al             ; 将设置写入键盘控制寄存器
    
    pop cx
    pop ax
    ret
buzzer endp  




;没有使用到es
;生成字符,高亮尽量使用20h
generate proc
	push bx
	push dx

	;这一部分给字符域赋值，并修改字符指针count
	mov range,27
	call rand
	lea bx,string
	;达到100自动回来
	call mod_count
	;用 byte count表示其中的字符偏移，加上之后就是字符的地址
	add bl,count
	adc bh,0
	add count,4
	add num,60h
	;设置要显示的小写字母
	mov dl,num
	MOV byte ptr[BX],dl

	;这一部分是给字符显示的属性域赋值，先字符域再属性域，最后位置域便于后续操作b800部分的内存
	mov byte ptr [bx+1],10;默认不高亮显示

	;设置字符的位置域
	mov range,80
	call rand
	;左移一位表示在第一行的位置，每行160字节
	SHL num,1
	;本来是应该把第三四字节看成整体的但是这里分开了，主要是num是字节类型，还要用 AX进行位扩展的话有点麻烦
	mov dl,num
	mov byte ptr [bx+2],dl
	mov dl,0
	mov byte ptr [bx+3],dl
	
	;收尾
	pop dx
	pop bx
	ret
generate endp

;参数 byte range为变量提前存好，作为上限值
;返回值放到变量 byte num 内
rand proc
	push ax
	push cx
	push dx
	
	mov ah,2ch
	int 21h
	mov AL,DL
	CBW
	MOV CX,0
	mov CL,range
	div cl
	mov num,ah
	
	pop dx
	pop cx
	pop ax
	ret
rand endp


;取余操作
mod_count proc
	cmp count,24
	jb mod_ret
	mov count, 0
mod_ret:
	ret
mod_count endp
  
;要转化的十进制数需放到bx内
BINTODEC proc 
 	push si
	push bx
 	push cx
 	push dx
 
 	
	mov si,0
	mov cx,10000D
	CALL dec_div
	mov cx,1000D
	call dec_div
	mov cx,100D
	call dec_div
	mov cx,10D
	call dec_div
	mov cx,1D
	call dec_div
  
  	pop dx
  	pop cx
  	pop bx
  	pop si
  	ret
BINTODEC endp
  
  
  
  
dec_div proc 
	mov ax,bx
	mov dx,0
	div cx
	mov bx,dx
	mov dl,al
	cmp si,0
	jz show_special
show_continue:	
	mov si,1
	add dl,30h
	mov AH,02h
	int 21h
	jmp show_ret
show_special:
	cmp dl,0
	jnz show_continue
show_ret:
	ret
dec_div endp

;返回值放到 bx 内
percent proc
	mov ax,score
	mov bx,100
	mul bx
	mov bx,click
	div bx
	mov bx,ax
	ret
percent endp




;安装自定义的9号中断的中断服务程序
install proc
	push ds
	push ax
	push es
	push si
	push di
	
	;DS存储  CS 的内容，方便把中断服务程序复制到系统中断向量区
	PUSH CS
	POP DS
 
	MOV AX,0
	MOV ES,AX

	;把9号中断的中断服务程序转移到204H（在空闲的中断向量区，系统和其他程序都用不到的区域）
	MOV SI,OFFSET INT9
	MOV DI,204H
	MOV CX, OFFSET INT9END - OFFSET INT9
	CLD
	REP MOVSB


	;利用地址200h和202h 保存原来9号键盘中断的中断服务例程
	PUSH ES:[36]
	POP  ES:[200H]
	PUSH ES:[38]
	POP  ES:[202H]

	;设置9号中断的中断服务程序
	CLI
	MOV WORD PTR ES:[36], 204H
	MOV WORD PTR ES:[38],0
	STI

	pop di
	pop si
	pop es
	pop ax
    pop ds
    ret
install endp
    
    
    
INT9:
	jmp short int9_start
	flag db 0
int9_start:
	PUSH AX
	
	;模仿中断调用：标志寄存器入栈，TF=0，IF=0 CS、IP入栈，根据中断向量表修改CS、IP
	;调用9号中断例程之前已经 cli 只需要标志寄存器入栈，CS、IP入栈，根据中断向量表修改CS、IP即可
	PUSHF
	CALL DWORD PTR CS:[200H]
	IN AL,60H
	cmp al,01H
	je int9_special
	jmp INT9RET
int9_special:
	mov byte ptr cs:[206h],1
	
	;测试发现已经成功地置为了1，并且正常地运行到了下方
	;mov dl,byte ptr cs:[206h]
	;add dl,30h
	;mov ah,02h
	;int 21h

INT9RET:
	POP AX
	IRET
INT9END: NOP


CODES ENDS
    END START
