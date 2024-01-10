program grid_tester

    use iric
    implicit none

    integer:: i, j

    integer:: file_id, ier
    integer:: icount, istatus
    integer:: canceled
    character(200)::condFile

    integer:: i_size, j_size  ! 格子点数
    integer:: n_node ! 格子点数

    ! 構造格子用の変数
    double precision, dimension(:, :), allocatable:: x_grid_s, y_grid_s
    double precision, dimension(:, :), allocatable:: elevation_s, Water_Surface_Elevation_s, Depth_S

    ! 非構造格子用の変数
    double precision, dimension(:), allocatable:: x_grid_u, y_grid_U
    double precision, dimension(:), allocatable:: elevation_u, Water_Surface_Elevation_u, Depth_u

    integer:: grid_type
    integer:: omake, Water_level_size, WS_Elv_adjustment
    double precision:: time, time_step, tmp_Water_level_value, WS_Elv_adjustment_value
    double precision, dimension(:), allocatable:: Water_level_time, Water_level_value

    time = 0

    write (*, '(a81)') '==================================program Start=================================='

    !--------------------------------------------------------------------------
    ! 計算データ(CGNSファイル)を開く(Intel Fortran Compiler用)
    !--------------------------------------------------------------------------
    write (*, *) '>>Start Opening CGNS File'

    ! コマンド名が数に含まれるので、引数が1つなら2を返す
    icount = nargs()

    ! ファイル名の取得
    if (icount .eq. 2) then
        call getarg(1, condFile, istatus)
    else
        write (*, *) "Input File not specified."
        stop
    end if

    ! 計算データ(CGNSファイル)を開く
    call cg_iric_open(condFile, IRIC_MODE_MODIFY, file_id, ier)
    if (ier /= 0) stop "*** Open error of CGNS file ***"

    write (*, '(a)') '    >Completed Opening CGNS File'

    !--------------------------------------------------------------------------
    ! 計算条件の読み込み
    !--------------------------------------------------------------------------
    write (*, *) '>>Start loading calculation conditions'

    ! 読み込む格子が構造格子か非構造格子かを読み込み
    call cg_iric_read_integer(file_id, "grid_type", grid_type, ier)

    if (grid_type == 0) then
        write (*, '(a)') '    >grid type is structured'
    else
        write (*, '(a)') '    >grid type is unstructured'
    end if

    ! オマケ機能を使用するかを読み込み
    call cg_iric_read_integer(file_id, "omake", omake, ier)

    if (omake == 0) then
        write (*, '(a)') '    >Normal mode is selected'
    else
        write (*, '(a)') '    >Omake mode is selected'
    end if

    ! 水位の補正の有無
    call cg_iric_read_real(file_id, "WS_Elv_adjustment_value", WS_Elv_adjustment_value, ier)

    write (*, '(a)') '    >Completed loading calculation conditions'

    !--------------------------------------------------------------------------
    ! 格子の読み込み
    !--------------------------------------------------------------------------
    write (*, *) '>>Start loading Grid conditions'

    if (grid_type == 0) then    !構造格子の場合

        ! 格子のサイズを取得
        call cg_iric_read_grid2d_str_size(file_id, i_size, j_size, ier)

        ! メモリの確保
        allocate (x_grid_s(i_size, j_size), y_grid_S(i_size, j_size))
        allocate (elevation_s(i_size, j_size))

        ! 格子点の座標読み込み
        call cg_iric_read_grid2d_coords(file_id, x_grid_s, y_grid_s, ier)
        call cg_iric_read_grid_real_node(file_id, "Elevation", elevation_s, ier)

    else   !非構造格子の場合

        ! 格子のサイズを取得
        call cg_iric_read_grid_nodecount(file_id, n_node, ier)

        ! メモリの確保
        allocate (x_grid_u(n_node), y_grid_u(n_node))
        allocate (elevation_u(n_node))

        ! 格子点の属性読み込み
        call cg_iric_read_grid2d_coords(file_id, x_grid_u, y_grid_u, ier)
        call cg_iric_read_grid_real_node(file_id, "Elevation", elevation_u, ier)

    end if

    write (*, '(a)') '    >Completed loading Grid conditions'

    !--------------------------------------------------------------------------
    ! 通常の出力かおまけモードかを呼びだす。
    !--------------------------------------------------------------------------

    if (omake == 0) then
        write (*, *) '>>run normal mode'
        call normal_calculation_and_output
    else
        write (*, *) '>>run omake mode'
        call omake_calculation_and_output
    end if

    !--------------------------------------------------------------------------
    ! 終了処理
    !--------------------------------------------------------------------------
    write (*, '(a81)') '================================Completed process================================'
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

        if (grid_type == 0) then
            ! 計算結果を出力
            call cg_iric_write_sol_node_real(file_id, "Elevation", elevation_s, ier)
        else
            ! 計算結果を出力
            call cg_iric_write_sol_node_real(file_id, "Elevation", elevation_u, ier)
        end if

        ! 時間ごとの出力を終了
        call cg_iric_write_sol_end(file_id, ier)

    end subroutine

    !==========================================================================
    ! おまけモード
    !==========================================================================
    subroutine omake_calculation_and_output

        call cg_iric_read_real(file_id, "time_step", time_step, ier)

        if (grid_type == 0) then

            allocate (Water_Surface_Elevation_s(i_size, j_size), Depth_s(i_size, j_size))

        else

            allocate (Water_Surface_Elevation_u(n_node), Depth_u(n_node))

        end if

        ! 関数型で入力した水位のサイズを読み込み
        call cg_iric_read_functionalsize(file_id, "Water_level", Water_level_size, ier)
        ! メモリ確保
        allocate (Water_level_time(Water_level_size))
        allocate (Water_level_value(Water_level_size))
        ! 水位のデータを読み込み
        call cg_iric_read_functional(file_id, "Water_level", Water_level_time, Water_level_value, ier)

        call cg_iric_read_integer(file_id, "WS_Elv_adjustment", WS_Elv_adjustment, ier)

        !--------------------------------------------------------------------------
        ! メインループ
        !--------------------------------------------------------------------------
        write (*, '(a81)') '*********************Start main roop of calculate and output*********************'

        do time = Water_level_time(1), Water_level_time(Water_level_size) + 1d-5, time_step
            !----------------------------------------------------------------------
            ! キャンセルが押されていないかを確認して押されていたらループ終了
            !----------------------------------------------------------------------
            call iric_check_cancel(canceled)
            if (canceled == 1) exit

            !----------------------------------------------------------------------
            ! 計算
            !----------------------------------------------------------------------
            do i = 2, Water_level_size
                if (time + 1d-5 >= Water_level_time(i - 1) .and. time <= Water_level_time(i) + 1d-5) then
                    tmp_Water_level_value = Water_level_value(i - 1) &
                                            + (time - Water_level_time(i - 1)) &
                                            *(Water_level_value(i) - Water_level_value(i - 1)) &
                                            /(Water_level_time(i) - Water_level_time(i - 1))
                end if
            end do

            write (*, '(a7,f9.3,a15,f8.3)') 'time = ', time - Water_level_time(1), 'water level = ', tmp_Water_level_value

            if (grid_type == 0) then       ! 構造格子の場合

                do i = 1, i_size
                    do j = 1, j_size

                        if (elevation_s(i, j) - tmp_Water_level_value > 1d-5) then
                            Water_Surface_Elevation_s(i, j) = elevation_s(i, j) - WS_Elv_adjustment_value
                        else
                            Water_Surface_Elevation_s(i, j) = tmp_Water_level_value
                        end if

                        if (Water_Surface_Elevation_s(i, j) - elevation_s(i, j) > 1d-5) then
                            Depth_s(i, j) = Water_Surface_Elevation_s(i, j) - elevation_s(i, j)
                        else
                            Depth_s(i, j) = 0.0
                        end if

                    end do
                end do

                if (WS_Elv_adjustment == 0) then
                    Water_Surface_Elevation_s = tmp_Water_level_value
                end if

            else    ! 非構造格子の場合

                do i = 1, n_node

                    if (elevation_u(i) - tmp_Water_level_value > 1d-5) then
                        Water_Surface_Elevation_u(i) = elevation_u(i) - WS_Elv_adjustment_value
                    else
                        Water_Surface_Elevation_u(i) = tmp_Water_level_value
                    end if

                    if (Water_Surface_Elevation_u(i) - elevation_u(i) > 1d-5) then
                        Depth_u(i) = Water_Surface_Elevation_u(i) - elevation_u(i)
                    else
                        Depth_u(i) = 0.0
                    end if

                end do

                if (WS_Elv_adjustment == 0) then
                    Water_Surface_Elevation_u = tmp_Water_level_value
                end if

            end if

            !----------------------------------------------------------------------
            ! 出力
            !----------------------------------------------------------------------
            ! 時間ごとの出力を開始
            call cg_iric_write_sol_start(file_id, ier)
            ! 時間を出力
            call cg_iric_write_sol_time(file_id, time - Water_level_time(1), ier)

            if (grid_type == 0) then
                ! 計算結果を出力
                call cg_iric_write_sol_node_real(file_id, "Elevation(m)", elevation_s, ier)
                call cg_iric_write_sol_node_real(file_id, "WaterSurfaceElevation(m)", Water_Surface_Elevation_s, ier)
                call cg_iric_write_sol_node_real(file_id, "Depth(m)", Depth_s, ier)
            else
                ! 計算結果を出力
                call cg_iric_write_sol_node_real(file_id, "Elevation(m)", elevation_u, ier)
                call cg_iric_write_sol_node_real(file_id, "WaterSurfaceElevation(m)", Water_Surface_Elevation_u, ier)
                call cg_iric_write_sol_node_real(file_id, "Depth(m)", Depth_u, ier)
            end if

            ! 時間ごとの出力を終了
            call cg_iric_write_sol_end(file_id, ier)

        end do

        write (*, '(a81)') '*********************************** Finish !! ***********************************'

    end subroutine

end program grid_tester
