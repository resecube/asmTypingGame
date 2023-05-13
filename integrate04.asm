DATAS SEGMENT
    string db 'a',20h,120,0,'b',20h,12h,01h,'c',20h,0ach,01h,12 DUP(0)
    speed db 6
    range db 0;���������������
    num db 0;���ɵ������
    input db '-';�û�������ַ�
    count db 12;��һ�������ɵ��ַ���ƫ�ƣ�ֻ��Ҫ8λ���ɣ�����string���100�ֽڣ�
    score dw 0;��ȷ�Ĵ���
    click dw 0;�������
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
    
    call percent;����ֱֵ����bx��
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
    
;����
clear proc
	push ax
	MOV AH, 00H  ; �ӹ��ܺ�
	MOV AL, 03H  ; �������ԣ��ڵװ��֣�
	INT 10H
	pop ax
	ret
clear endp




;ˢ����Ļ
;ʹ����es���������ֳ�����
refresh proc
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	;����
	call clear

	mov ax,0b800h
	mov es,ax
	lea si,string
	
	;ѭ������---�ҵ�λ��/�����ַ�������/����λ��
	mov cx,6
refresh1:
	mov dx,[si]
	cmp dl,61h;;;;;;;;;
	jb refresh_special;����ַ���ascii��С��61h�Ļ����Ͳ���ʾ�ˣ�˵������Ч�ַ�
	mov di,[si+2]
	cmp di,4000
	ja refresh_special;������Դ�����ƫ�ƴ���4000��������ʾ��ֱ����һ��
	add word ptr [si+2],160;ʹ�´���ʾʱ����
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
 	
    MOV AH, 01h   ; ����AH�Ĵ���Ϊ01h����ʾ�����̻�����
    INT 16h       ; ����BIOS�ж�16h�������̻�����
    JZ delay_continue ; ������̻�����Ϊ�գ�������click����


	inc click
    MOV AH, 00h   ; ����AH�Ĵ���Ϊ00h����ʾ��ȡ����ɨ����
    INT 16h       ; ����BIOS�ж�16h����ȡ����ɨ����
	
	;���ַ���ASCII�����input����check�ӿ�
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
  	ret             ; ����
SLEEP ENDP



;�Ƚ����޸ĺ�����ǰ����input��������ASCII��
;���� byte input ��word scoreΪ��Եķ���
;ֻ�޸���stringû��ʹ��es
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
	mov byte ptr [bx+1],20h;ǰ��ɫ��ʾΪ��ɫ
	inc score
	call buzzer
check_ret:
	pop dx
	pop cx
	pop bx
	ret
check endp
    
    
;ʹ��������������
buzzer proc
    push ax
    push cx
    
    mov al, 182             ; ���÷���jkģʽ
    out 43h, al             ; ������д����ƼĴ���
    
    mov ax, 1           ; ���ü�������ֵ
    out 42h, al             ; ����8λд�������
    mov al, ah
    out 42h, al             ; ����8λд�������
    
    in al, 61h              ; ��ȡ���̿��ƼĴ���
    or al, 3                ; ��PC���ȵĿ���λ
    out 61h, al             ; ������д����̿��ƼĴ���
    
    mov cx, 5000            ; ��ʱԼ0.1��
delay1:
    loop delay1              ; ѭ����ʱ
    
    in al, 61h              ; �ر�PC���ȵĿ���λ
    and al, 11111100b
    out 61h, al             ; ������д����̿��ƼĴ���
    
    pop cx
    pop ax
    ret
buzzer endp  




;û��ʹ�õ�es
;�����ַ�,��������ʹ��20h
generate proc
	push bx
	push dx

	;��һ���ָ��ַ���ֵ�����޸��ַ�ָ��count
	mov range,27
	call rand
	lea bx,string
	;�ﵽ100�Զ�����
	call mod_count
	;�� byte count��ʾ���е��ַ�ƫ�ƣ�����֮������ַ��ĵ�ַ
	add bl,count
	adc bh,0
	add count,4
	add num,60h
	;����Ҫ��ʾ��Сд��ĸ
	mov dl,num
	MOV byte ptr[BX],dl

	;��һ�����Ǹ��ַ���ʾ��������ֵ�����ַ��������������λ������ں�������b800���ֵ��ڴ�
	mov byte ptr [bx+1],10;Ĭ�ϲ�������ʾ

	;�����ַ���λ����
	mov range,80
	call rand
	;����һλ��ʾ�ڵ�һ�е�λ�ã�ÿ��160�ֽ�
	SHL num,1
	;������Ӧ�ðѵ������ֽڿ�������ĵ�������ֿ��ˣ���Ҫ��num���ֽ����ͣ���Ҫ�� AX����λ��չ�Ļ��е��鷳
	mov dl,num
	mov byte ptr [bx+2],dl
	mov dl,0
	mov byte ptr [bx+3],dl
	
	;��β
	pop dx
	pop bx
	ret
generate endp

;���� byte rangeΪ������ǰ��ã���Ϊ����ֵ
;����ֵ�ŵ����� byte num ��
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


;ȡ�����
mod_count proc
	cmp count,24
	jb mod_ret
	mov count, 0
mod_ret:
	ret
mod_count endp
  
;Ҫת����ʮ��������ŵ�bx��
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

;����ֵ�ŵ� bx ��
percent proc
	mov ax,score
	mov bx,100
	mul bx
	mov bx,click
	div bx
	mov bx,ax
	ret
percent endp




;��װ�Զ����9���жϵ��жϷ������
install proc
	push ds
	push ax
	push es
	push si
	push di
	
	;DS�洢  CS �����ݣ�������жϷ�������Ƶ�ϵͳ�ж�������
	PUSH CS
	POP DS
 
	MOV AX,0
	MOV ES,AX

	;��9���жϵ��жϷ������ת�Ƶ�204H���ڿ��е��ж���������ϵͳ�����������ò���������
	MOV SI,OFFSET INT9
	MOV DI,204H
	MOV CX, OFFSET INT9END - OFFSET INT9
	CLD
	REP MOVSB


	;���õ�ַ200h��202h ����ԭ��9�ż����жϵ��жϷ�������
	PUSH ES:[36]
	POP  ES:[200H]
	PUSH ES:[38]
	POP  ES:[202H]

	;����9���жϵ��жϷ������
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
	
	;ģ���жϵ��ã���־�Ĵ�����ջ��TF=0��IF=0 CS��IP��ջ�������ж��������޸�CS��IP
	;����9���ж�����֮ǰ�Ѿ� cli ֻ��Ҫ��־�Ĵ�����ջ��CS��IP��ջ�������ж��������޸�CS��IP����
	PUSHF
	CALL DWORD PTR CS:[200H]
	IN AL,60H
	cmp al,01H
	je int9_special
	jmp INT9RET
int9_special:
	mov byte ptr cs:[206h],1
	
	;���Է����Ѿ��ɹ�����Ϊ��1���������������е����·�
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
