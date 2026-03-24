

// TypeScript declarations for @stdlib/lapack/base/dtgsy2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves the generalized Sylvester equation (unblocked)
	*/
	(
		trans: string,
		ijob: number,
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		D: Float64Array,
		strideD1: number,
		strideD2: number,
		offsetD: number,
		E: Float64Array,
		strideE1: number,
		strideE2: number,
		offsetE: number,
		F: Float64Array,
		strideF1: number,
		strideF2: number,
		offsetF: number,
		scale: number,
		rdsum: number,
		rdscal: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number,
		pq: number
	): Float64Array;
}

/**
* Solves the generalized Sylvester equation (unblocked)
*/
declare var dtgsy2: Routine;

export = dtgsy2;
