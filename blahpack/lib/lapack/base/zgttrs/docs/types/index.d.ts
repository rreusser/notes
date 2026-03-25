

// TypeScript declarations for @stdlib/lapack/base/zgttrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve tridiagonal system using LU factorization (complex)
	*/
	(
		trans: string,
		N: number,
		nrhs: number,
		DL: Float64Array,
		strideDL: number,
		offsetDL: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		DU: Float64Array,
		strideDU: number,
		offsetDU: number,
		DU2: Float64Array,
		strideDU2: number,
		offsetDU2: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solve tridiagonal system using LU factorization (complex)
*/
declare var zgttrs: Routine;

export = zgttrs;
