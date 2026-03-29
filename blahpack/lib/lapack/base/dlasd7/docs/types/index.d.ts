

// TypeScript declarations for @stdlib/lapack/base/dlasd7

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Merge two sets of singular values together into a single sorted set and deflate.
	*/
	(
		icompq: number,
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
		ZW: Float64Array,
		strideZW: number,
		offsetZW: number,
		VF: Float64Array,
		strideVF: number,
		offsetVF: number,
		VFW: Float64Array,
		strideVFW: number,
		offsetVFW: number,
		VL: Float64Array,
		strideVL: number,
		offsetVL: number,
		VLW: Float64Array,
		strideVLW: number,
		offsetVLW: number,
		alpha: number,
		beta: number,
		DSIGMA: Float64Array,
		strideDSIGMA: number,
		offsetDSIGMA: number,
		IDX: Int32Array,
		strideIDX: number,
		offsetIDX: number,
		IDXP: Int32Array,
		strideIDXP: number,
		offsetIDXP: number,
		IDXQ: Int32Array,
		strideIDXQ: number,
		offsetIDXQ: number,
		PERM: Int32Array,
		stridePERM: number,
		offsetPERM: number,
		givptr: number,
		GIVCOL: Int32Array,
		strideGIVCOL1: number,
		strideGIVCOL2: number,
		offsetGIVCOL: number,
		GIVNUM: Float64Array,
		strideGIVNUM1: number,
		strideGIVNUM2: number,
		offsetGIVNUM: number,
		c: number,
		s: number
	): Float64Array;
}

/**
* Merge two sets of singular values together into a single sorted set and deflate.
*/
declare var dlasd7: Routine;

export = dlasd7;
