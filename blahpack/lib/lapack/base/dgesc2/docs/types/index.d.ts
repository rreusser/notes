

// TypeScript declarations for @stdlib/lapack/base/dgesc2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a system of linear equations with an LU factored matrix using complete pivoting
	*/
	(
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		RHS: Float64Array,
		strideRHS: number,
		offsetRHS: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		JPIV: Int32Array,
		strideJPIV: number,
		offsetJPIV: number,
		scale: number
	): Float64Array;
}

/**
* Solves a system of linear equations with an LU factored matrix using complete pivoting
*/
declare var dgesc2: Routine;

export = dgesc2;
