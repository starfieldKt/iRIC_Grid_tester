program grid_tester
    
    use iric
    implicit none

    integer:: file_id, ier
    integer:: icount, istatus
    integer:: canceled
    character(200)::condFile

    integer:: i_size, j_size
    double precision, dimension(:,:), allocatable:: x_grid, y_grid
    double precision, dimension(:,:), allocatable:: elevation

    double precision:: time, time_step, time_end
    time = 0

    write(*,'(a81)') '==================================program Start=================================='

    !--------------------------------------------------------------------------
    ! 計算データ(CGNSファイル)を開く(Intel Fortran Compiler用)
    !--------------------------------------------------------------------------
    write(*,*) '>>Start Opening CGNS File'

    ! コマンド名が数に含まれるので、引数が1つなら2を返す
    icount = nargs()                            
    
    ! ファイル名の取得
    if ( icount.eq.2 ) then
        call getarg(1, condFile, istatus)
    else
        write(*,*) "Input File not specified."
        stop
    endif

    ! 計算データ(CGNSファイル)を開く
    call cg_iric_open(condFile, IRIC_MODE_MODIFY, file_id, ier)
    if (ier /=0) stop "*** Open error of CGNS file ***"

    write(*,'(a)') '    Completed Opening CGNS File'

    !--------------------------------------------------------------------------
    ! 計算条件の読み込み
    !--------------------------------------------------------------------------
    write(*,*) '>>Start loading calculation conditions'
    
    call cg_iric_read_real(file_id, "time_step", time_step, ier)
    call cg_iric_read_real(file_id, "time_end", time_end, ier)

    write(*,'(a)') '        Completed loading calculation conditions'
    !--------------------------------------------------------------------------
    ! 格子の読み込み
    !--------------------------------------------------------------------------
    write(*,*) '>>Start loading Grid conditions'

    ! 格子のサイズを取得
    call cg_iric_read_grid2d_str_size(file_id, i_size, j_size, ier)

    ! メモリの確保
    allocate(x_grid(i_size, j_size), y_grid(i_size, j_size))
    allocate(elevation(i_size, j_size))

    ! 格子点の座標読み込み
    call cg_iric_read_grid2d_coords(file_id, x_grid, y_grid, ier)

    ! 格子点の属性読み込み
    call cg_iric_read_grid_real_node(file_id, "Elevation", elevation, ier)

    write(*,'(a)') '    Completed loading Grid conditions'

    !--------------------------------------------------------------------------
    ! メインループ
    !--------------------------------------------------------------------------
    write(*,'(a81)') '*********************Start main roop of calculate and output*********************'

    do time = 0, time_end, time_step
        !----------------------------------------------------------------------
        ! キャンセルが押されていないかを確認して押されていたらループ終了
        !----------------------------------------------------------------------
        call iric_check_cancel(canceled)
        if (canceled == 1) exit

        !----------------------------------------------------------------------
        ! 計算
        !----------------------------------------------------------------------

        !今回は何も計算なし！

        !----------------------------------------------------------------------
        ! 出力
        !----------------------------------------------------------------------
        ! 時間ごとの出力を開始
        call cg_iric_write_sol_start(file_id, ier)
        ! 時間を出力
        call cg_iric_write_sol_time(file_id, time, ier)

        ! 格子を出力
        call cg_iric_write_sol_grid2d_coords(file_id, x_grid, y_grid, ier)

        ! 計算結果を出力
        call cg_iric_write_sol_node_real(file_id, "Elevation", elevation, ier)

        ! 時間ごとの出力を終了
        call cg_iric_write_sol_end(file_id, ier)

    end do

    write(*,'(a81)') '*********************************** Finish !! ***********************************'

    !--------------------------------------------------------------------------
    ! 終了処理
    !--------------------------------------------------------------------------
    ! 計算データファイルを閉じる
    call cg_iric_close(file_id, ier)
    stop

end program grid_tester