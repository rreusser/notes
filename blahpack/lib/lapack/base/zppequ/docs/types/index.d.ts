

// TypeScript declarations for @stdlib/lapack/base/zppequ

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute row and column scalings to equilibrate a complex Hermitian positive definite matrix in packed storage
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		scond: number,
		amax: number
	): Float64Array;
}

/**
* Compute row and column scalings to equilibrate a complex Hermitian positive definite matrix in packed storage
*/
declare var zppequ: Routine;

export = zppequ;
