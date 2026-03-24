

// TypeScript declarations for @stdlib/lapack/base/dlaebz

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Auxiliary bisection routine for tridiagonal eigenvalue computation
	*/
	(
		ijob: number,
		nitmax: number,
		N: number,
		minp: number,
		nbmin: number,
		abstol: number,
		reltol: number,
		pivmin: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		E2: Float64Array,
		strideE2: number,
		offsetE2: number,
		NVAL: Int32Array,
		strideNVAL: number,
		offsetNVAL: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		mout: number,
		NAB: Int32Array,
		strideNAB1: number,
		strideNAB2: number,
		offsetNAB: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Auxiliary bisection routine for tridiagonal eigenvalue computation
*/
declare var dlaebz: Routine;

export = dlaebz;
