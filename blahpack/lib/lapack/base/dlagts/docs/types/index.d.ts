

// TypeScript declarations for @stdlib/lapack/base/dlagts

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a tridiagonal system factored by dlagtf
	*/
	(
		job: number,
		N: number,
		a: Float64Array,
		strideA: number,
		offsetA: number,
		b: Float64Array,
		strideB: number,
		offsetB: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		IN: Int32Array,
		strideIN: number,
		offsetIN: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		tol: number
	): Float64Array;
}

/**
* Solves a tridiagonal system factored by dlagtf
*/
declare var dlagts: Routine;

export = dlagts;
