

// TypeScript declarations for @stdlib/lapack/base/dgtts2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a real tridiagonal system using LU factorization from dgttrf (unblocked)
	*/
	(
		itrans: number,
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
* Solves a real tridiagonal system using LU factorization from dgttrf (unblocked)
*/
declare var dgtts2: Routine;

export = dgtts2;
