program grid_tester
    
    use iric
    implicit none

    integer:: i, j

    integer:: file_id, ier
    integer:: icount, istatus
    integer:: canceled
    character(200)::condFile

    integer:: i_size, j_size
    double precision, dimension(:,:), allocatable:: x_grid, y_grid
    double precision, dimension(:,:), allocatable:: elevation

    integer:: omake, Water_level_size, WS_Elv_adjustment
    double precision:: time, time_step ,tmp_Water_level_value
    double precision, dimension(:), allocatable:: Water_level_time, Water_level_value
    double precision, dimension(:,:), allocatable:: Water_Surface_Elevation, Depth
    
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

    write(*,'(a)') '    >Completed Opening CGNS File'

    !--------------------------------------------------------------------------
    ! 計算条件の読み込み
    !--------------------------------------------------------------------------
    write(*,*) '>>Start loading calculation conditions'
    
    call cg_iric_read_integer(file_id, "omake", omake, ier)

    if (omake == 0 ) then
        write(*,'(a)') '    >Normal mode is selected'
    else
        write(*,'(a)') '    >Omake mode is selected'
    end if

    write(*,'(a)') '    >Completed loading calculation conditions'

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

    write(*,'(a)') '    >Completed loading Grid conditions'

    !--------------------------------------------------------------------------
    ! 通常の出力かおまけモードかを呼びだす。
    !--------------------------------------------------------------------------

    if (omake == 0) then
        write(*,*) '>>run normal mode'
        call normal_calculation_and_output
    else
        write(*,*) '>>run omake mode'
        call omake_calculation_and_output
    end if

    !--------------------------------------------------------------------------
    ! 終了処理
    !--------------------------------------------------------------------------
    write(*,'(a81)') '================================Completed process================================'
    ! 計算データファイルを閉じる
    call cg_iric_close(file_id, ier)
    stop

contains

    !==========================================================================
    ! 通常モード
    !==========================================================================
    subroutine normal_calculation_and_output

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

    end subroutine

    !==========================================================================
    ! おまけモード
    !==========================================================================
    subroutine omake_calculation_and_output

        call cg_iric_read_real(file_id, "time_step", time_step, ier)

        allocate(Water_Surface_Elevation(i_size, j_size), Depth(i_size, j_size))

        ! 関数型で入力した水位のサイズを読み込み
        call cg_iric_read_functionalsize(file_id, "Water_level", Water_level_size, ier)
        ! メモリ確保
        allocate(Water_level_time(Water_level_size))
        allocate(Water_level_value(Water_level_size))
        ! 水位のデータを読み込み
        call cg_iric_read_functional(file_id, "Water_level", Water_level_time, Water_level_value, ier)

        call cg_iric_read_integer(file_id, "WS_Elv_adjustment", WS_Elv_adjustment, ier)

        !--------------------------------------------------------------------------
        ! メインループ
        !--------------------------------------------------------------------------
        write(*,'(a81)') '*********************Start main roop of calculate and output*********************'

        do time = Water_level_time(1), Water_level_time(Water_level_size), time_step
            !----------------------------------------------------------------------
            ! キャンセルが押されていないかを確認して押されていたらループ終了
            !----------------------------------------------------------------------
            call iric_check_cancel(canceled)
            if (canceled == 1) exit

            !----------------------------------------------------------------------
            ! 計算
            !----------------------------------------------------------------------
            do i = 2,  Water_level_size
                if (time >= Water_level_time( i - 1 ) .and. time <= Water_level_time( i )) then
                    tmp_Water_level_value = Water_level_value(i-1) &
                                          + (time-Water_level_time( i - 1 )) & 
                                          * (Water_level_value(i)-Water_level_value(i-1)) &
                                          / (Water_level_time(i)-Water_level_time(i-1))
                end if
            end do

            write(*,'(a7,f8.2,a15,f8.3)') 'time = ', time, 'water level = ', tmp_Water_level_value
            
            do i = 1, i_size
                do j = 1, j_size
                    if ( elevation(i,j) > tmp_Water_level_value) then
                        Water_Surface_Elevation(i,j) = elevation(i,j)
                    else
                        Water_Surface_Elevation(i,j) = tmp_Water_level_value
                    end if

                    Depth(i,j) = Water_Surface_Elevation(i,j) - elevation(i,j)

                end do
            end do

            if (WS_Elv_adjustment == 0) then
                Water_Surface_Elevation = tmp_Water_level_value
            end if

            !----------------------------------------------------------------------
            ! 出力
            !----------------------------------------------------------------------
            ! 時間ごとの出力を開始
            call cg_iric_write_sol_start(file_id, ier)
            ! 時間を出力
            call cg_iric_write_sol_time(file_id, time - Water_level_time(1), ier)

            ! 格子を出力
            call cg_iric_write_sol_grid2d_coords(file_id, x_grid, y_grid, ier)

            ! 計算結果を出力
            call cg_iric_write_sol_node_real(file_id, "Elevation(m)", elevation, ier)
            call cg_iric_write_sol_node_real(file_id, "WaterSurfaceElevation(m)", Water_Surface_Elevation, ier)
            call cg_iric_write_sol_node_real(file_id, "Depth(m)", Depth, ier)

            ! 時間ごとの出力を終了
            call cg_iric_write_sol_end(file_id, ier)

        end do

        write(*,'(a81)') '*********************************** Finish !! ***********************************'

    end subroutine

end program grid_tester