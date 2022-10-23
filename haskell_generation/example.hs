{-# LANGUAGE LambdaCase #-}

initial_a_coeffs = ( 1, 0 )
	:: ( Int, Int )

initial_b_coeffs = ( 0, 1 )
	:: ( Int, Int )

extended_euclidean = ee_recursion initial_a_coeffs initial_b_coeffs
	:: Int -> Int -> ( Int, Int, Int )

ee_recursion = ( \a_coeffs -> \b_coeffs -> \x -> \case
	0 -> ( x, get_first a_coeffs, get_first b_coeffs )
	y -> 
		let
		compute_new = ( \( prev_prev, prev ) -> ( prev, prev_prev - div x y * prev )
	) :: ( Int, Int ) -> ( Int, Int )

		new_a_coeffs = compute_new a_coeffs
	:: ( Int, Int )

		new_b_coeffs = compute_new b_coeffs
	:: ( Int, Int )


		in
ee_recursion new_a_coeffs new_b_coeffs y (mod x y)

	) :: ( Int, Int ) -> ( Int, Int ) -> Int -> Int -> ( Int, Int, Int )

