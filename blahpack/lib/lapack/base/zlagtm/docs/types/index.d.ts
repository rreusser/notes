

// TypeScript declarations for @stdlib/lapack/base/zlagtm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform a matrix-vector product of the form C := alpha*A*B + beta*C where A is a complex tridiagonal matrix.
	*/
	(
		trans: string,
		N: number,
		nrhs: number,
		alpha: number,
		DL: Float64Array,
		strideDL: number,
		offsetDL: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		DU: Float64Array,
		strideDU: number,
		offsetDU: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
		beta: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Perform a matrix-vector product of the form C := alpha*A*B + beta*C where A is a complex tridiagonal matrix.
*/
declare var zlagtm: Routine;

export = zlagtm;
