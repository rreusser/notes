

// TypeScript declarations for @stdlib/lapack/base/dlasd2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Merge two sets of singular values in bidiagonal SVD divide and conquer
	*/
	(
		nl: number,
		nr: number,
		sqre: number,
		K: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		z: Float64Array,
		strideZ: number,
		offsetZ: number,
		alpha: number,
		beta: number,
		U: Float64Array,
		strideU1: number,
		strideU2: number,
		offsetU: number,
		VT: Float64Array,
		strideVT1: number,
		strideVT2: number,
		offsetVT: number,
		DSIGMA: Float64Array,
		strideDSIGMA: number,
		offsetDSIGMA: number,
		U2: Float64Array,
		strideU21: number,
		strideU22: number,
		offsetU2: number,
		VT2: Float64Array,
		strideVT21: number,
		strideVT22: number,
		offsetVT2: number,
		IDXP: Int32Array,
		strideIDXP: number,
		offsetIDXP: number,
		IDX: Int32Array,
		strideIDX: number,
		offsetIDX: number,
		IDXC: Int32Array,
		strideIDXC: number,
		offsetIDXC: number,
		IDXQ: Int32Array,
		strideIDXQ: number,
		offsetIDXQ: number,
		COLTYP: Int32Array,
		strideCOLTYP: number,
		offsetCOLTYP: number
	): Float64Array;
}

/**
* Merge two sets of singular values in bidiagonal SVD divide and conquer
*/
declare var dlasd2: Routine;

export = dlasd2;
