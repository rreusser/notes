

// TypeScript declarations for @stdlib/lapack/base/dgees

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes eigenvalues and Schur decomposition of a real general matrix
	*/
	(
		jobvs: string,
		sort: string,
		select: boolean,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		sdim: number,
		WR: Float64Array,
		strideWR: number,
		offsetWR: number,
		WI: Float64Array,
		strideWI: number,
		offsetWI: number,
		VS: Float64Array,
		strideVS1: number,
		strideVS2: number,
		offsetVS: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		BWORK: Float64Array,
		strideBWORK: number,
		offsetBWORK: number
	): Float64Array;
}

/**
* Computes eigenvalues and Schur decomposition of a real general matrix
*/
declare var dgees: Routine;

export = dgees;
