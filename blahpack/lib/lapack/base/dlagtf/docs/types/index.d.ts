

// TypeScript declarations for @stdlib/lapack/base/dlagtf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Factorizes the matrix (T - lambda*I) where T is a tridiagonal matrix
	*/
	(
		N: number,
		a: Float64Array,
		strideA: number,
		offsetA: number,
		lambda: number,
		b: Float64Array,
		strideB: number,
		offsetB: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		tol: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		IN: Int32Array,
		strideIN: number,
		offsetIN: number
	): Float64Array;
}

/**
* Factorizes the matrix (T - lambda*I) where T is a tridiagonal matrix
*/
declare var dlagtf: Routine;

export = dlagtf;
