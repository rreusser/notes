

// TypeScript declarations for @stdlib/lapack/base/dggsvp3

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the preprocessing for the generalized SVD of real matrices A and B
	*/
	(
		jobu: string,
		jobv: string,
		jobq: string,
		M: number,
		p: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		tola: number,
		tolb: number,
		K: number,
		l: number,
		U: Float64Array,
		strideU1: number,
		strideU2: number,
		offsetU: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Compute the preprocessing for the generalized SVD of real matrices A and B
*/
declare var dggsvp3: Routine;

export = dggsvp3;
