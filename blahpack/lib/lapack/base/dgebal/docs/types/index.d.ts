

// TypeScript declarations for @stdlib/lapack/base/dgebal

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Balances a general real matrix for eigenvalue computation
	*/
	(
		job: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		ilo: number,
		ihi: number,
		SCALE: Float64Array,
		strideSCALE: number,
		offsetSCALE: number
	): Float64Array;
}

/**
* Balances a general real matrix for eigenvalue computation
*/
declare var dgebal: Routine;

export = dgebal;
