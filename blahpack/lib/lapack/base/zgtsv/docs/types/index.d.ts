

// TypeScript declarations for @stdlib/lapack/base/zgtsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a complex general tridiagonal system of linear equations A * X = B
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
* Solve a complex general tridiagonal system of linear equations A * X = B
*/
declare var zgtsv: Routine;

export = zgtsv;
