

// TypeScript declarations for @stdlib/lapack/base/dlarrj

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Refine eigenvalue approximations using bisection given initial intervals.
	*/
	(
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		E2: Float64Array,
		strideE2: number,
		offsetE2: number,
		ifirst: number,
		ilast: number,
		rtol: number,
		offset: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		WERR: Float64Array,
		strideWERR: number,
		offsetWERR: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number,
		pivmin: number,
		spdiam: number
	): Float64Array;
}

/**
* Refine eigenvalue approximations using bisection given initial intervals.
*/
declare var dlarrj: Routine;

export = dlarrj;
