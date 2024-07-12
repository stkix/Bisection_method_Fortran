program bisection_method
  implicit none
  real(8) ::x1, x2, accuracy, answer
  integer :: error
  character(len=40)::action

! エラー検出のため初期値0を設定する
  error = 0
! 精度値を設定する
  accuracy = 1.0e-9

!範囲を設定する
  print *,'x1='
  read *,x1
  write(*,'(30("-"))')
  
  print *,'x2='
  read *,x2
  write(*,'(30("-"))')

! 二分法の関数
  call bisection(x1, x2, accuracy, answer, error)

! エラー表示が0であれば解を表示する
  select case(error) ! エラーが検出されればエラー値（1or2）を表示

  !エラー値が1の場合
  case(1)
    print *, 'ERROR=', error  
    write(*,'(150("-"))')
    print *,'x1とx2が同じ符号であるため二分法の計算ができない'
    write(*,'(150("-"))')
    print *,'もし計算を終了したい場合はexitと入力してください。'
    print *,'終了したくない場合はdoと入力してください'
    read *,action
    if (action=='exit') then !exitの場合計算終了
      write(*,'(150("-"))')
      print *,'計算終了しました。'
    error=4
    !計算完了を示し、エラー値を4にする。
    end if
    if(action=='do') then !doの場合計算続行
      print *,'それではx1とx2を入力してください'
      print *,'x1='
      read *,x1
      write(*,'(30("-"))')
      print *,'x2='
      read *,x2
      write(*,'(30("-"))')
      error=0
      !エラーの初期値を0にする。
      call bisection(x1, x2, accuracy, answer, error)
      write(*,'(150("-"))')
      if (error==0) then
        goto 0003
        !0003とは結果表示
      end if
      if (error==1)then
        goto 0001
        !エラー値が1の時の処理に行く
      end if
      if (error==2)then
        goto 0002
        !エラー値が2の時の処理に行く
      end if
    end if

 !エラー値が2の場合
  case(2)
    print *, 'ERROR=', error  ! エラーが検出されればエラー値（1or2）を表示
    print *,'求める解は完璧な解ではないが、その解と近い解を求めた'
    print *,'その近い解は',answer,'である'
    print *,'もし計算を終了したい場合はexitと入力してください。'
    print *,'終了したくない場合はdoと入力してください'
    read *,action
    if (action=='exit') then !exitの場合計算終了
      write(*,'(150("-"))')
      print *,'計算終了しました。'
      error=4
      !計算完了を示し、エラー値を4にする
    end if
    if(action=='do') then !doの場合計算続行
      print *,'それではx1とx2を入力してください'
      print *,'x1='
      read *,x1
      write(*,'(30("-"))')
      print *,'x2='
      read *,x2
      write(*,'(30("-"))')
      error=0
      !エラーの初期値を0にする
      call bisection(x1, x2, accuracy, answer, error)
      write(*,'(150("-"))')
      if (error==0) then
        goto 0003
        !0003とは結果表示
      end if
      if (error==1)then
        goto 0001
        !エラー値が1の時の処理に行く
      end if
      if (error==2)then
        goto 0002
        !エラー値が2の時の処理に行く
      end if
    end if
  end select



  !0001,0002,0003における処理


      !エラー値が1の時の処理
      0001 if (error==1) then
      print *, 'ERROR=', error  
       write(*,'(150("-"))')
       print *,'x1とx2が同じ符号であるため二分法の計算ができない'
       write(*,'(150("-"))')
       print *,'もし計算を終了したい場合はexitと入力してください。'
       print *,'終了したくない場合はdoと入力してください'
       read *,action
       if (action=='exit') then
         write(*,'(150("-"))')
         print *,'計算終了しました。'
         error=4
         !計算完了を示し、エラー値を4にする
       end if
       if(action=='do') then
         print *,'それではx1とx2を入力してください'
         print *,'x1='
         read *,x1
         write(*,'(30("-"))')
         print *,'x2='
         read *,x2
         write(*,'(30("-"))')
         error=0
         !エラーの初期値を0にする
         call bisection(x1, x2, accuracy, answer, error)
         write(*,'(150("-"))')
         if (error==0) then
           goto 0003
           !0003とは結果表示
         end if
         if (error==1)then
           goto 0001
           !エラー値が1の時の処理に行く
         end if
         if (error==2)then
           goto 0002
           !エラー値が2の時の処理に行く
         end if
       end if
      end if

      !エラー値が2の時の処理
      0002 if (error==2)then
      print *, 'ERROR=', error  ! エラーが検出されればエラー値（1or2）を表示
       print *,'求める解は完璧な解ではないが、その解と近い解を求めた'
       print *,'その近い解は',answer,'である'
       print *,'もし計算を終了したい場合はexitと入力してください。'
       print *,'終了したくない場合はdoと入力してください'
       read *,action
       if (action=='exit') then
         write(*,'(150("-"))')
         print *,'計算終了しました。'
         error=4
         !計算完了を示し、エラー値を4にする
       end if
       if(action=='do') then
         print *,'それではx1とx2を入力してください'
         print *,'x1='
         read *,x1
         write(*,'(30("-"))')
         print *,'x2='
         read *,x2
         write(*,'(30("-"))')
         error=0
         !エラーの初期値を0にする
         call bisection(x1, x2, accuracy, answer, error)
         write(*,'(150("-"))')
         if (error==0) then
           goto 0003
           !0003とは結果表示
         end if
         if (error==1)then
           goto 0001
           !エラー値が1の時の処理に行く
         end if
         if (error==2)then
           goto 0002
           !エラー値が2の時の処理に行く
         end if
       end if
      end if

      !0003で結果表示
      0003 if (error==0) then
        write(*,'(150("-"))') 
        print *,'答えAnswer=',answer
        print *,'計算終了しました'
      end if

 
end program bisection_method



!サブプログラム

!サブルーチンを用いて関数を定義する
subroutine bisection(x1, x2, accuracy, answer, error)
  implicit none
  real(8) :: x1, x2, accuracy, answer
  integer :: error

  real(8) :: f1, f2, xp, xn, xmid, fmid
  integer :: i,n
! yの値を計算
  call func(x1, f1)
  call func(x2, f2)
  if (f1*f2 > 0.0) then
! エラー値を１に設定（解がある保証がない）（二分法において二つ同符号の場合計算できない）
    error = 1
! subroutineからメインプログラムへ返る
    return
  endif

  !f1が0の時、x1はanswerである。
  if (f1==0)then
    answer = x1
    error = 0
    return
  end if

  !f2が0の時、x2はanswerである。
  if (f2==0)then
    answer = x2
    error = 0
    return
  end if

!xpはfxが正を取る時のxの値、xnはfxが負を取る時のxの値
  if (f1 > 0.0) then
    xp = x1
    xn = x2
  else
    xp = x2
    xn = x1
  endif

!計算の回数を設定する
  print *,'計算の回数nはn='
  read *,n
  write(*,'(30("-"))')

  do i=1, n
    xmid = (xp + xn)/2.0
    print *,'xの中間値', xmid,'xの正の値', xp,'xの負の値', xn,'xの範囲の幅', ABS(xp-xn)
!-- 精度が十分であればanswerに値を代入してメインプログラムに返る
    if (ABS(xp-xn) < accuracy) then
      answer = xmid
      return
    endif
    call func(xmid, fmid)
    print *,'x=',xmid,'の時fx=', fmid
    write(*,'(150("-"))') 
    if (fmid > 0.0) xp = xmid
    if (fmid < 0.0) xn = xmid
    if (fmid == 0.0) then
      answer = xmid
      return
    endif
  enddo
!-- エラー値を2に設定（精度を満たすことに失敗）
  error = 2
!-- 精度を満たせなかったため、中間値を解とする
  answer = (xp + xn)/2.0
end subroutine bisection

!関数を定義する
subroutine func(x, f)
  implicit none
  real(8) x, f
  f = 4*x**3-2*x**2-6*x+3
end subroutine func