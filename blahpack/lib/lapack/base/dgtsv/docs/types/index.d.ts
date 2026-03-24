

// TypeScript declarations for @stdlib/lapack/base/dgtsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a general real tridiagonal system of linear equations
	*/
	(
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
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solves a general real tridiagonal system of linear equations
*/
declare var dgtsv: Routine;

export = dgtsv;
