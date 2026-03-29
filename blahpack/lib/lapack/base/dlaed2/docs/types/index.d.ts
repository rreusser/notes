

// TypeScript declarations for @stdlib/lapack/base/dlaed2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Merge eigenvalues and deflate secular equation in divide and conquer
	*/
	(
		K: number,
		N: number,
		n1: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		INDXQ: Int32Array,
		strideINDXQ: number,
		offsetINDXQ: number,
		rho: number,
		z: Float64Array,
		strideZ: number,
		offsetZ: number,
		DLAMBDA: Float64Array,
		strideDLAMBDA: number,
		offsetDLAMBDA: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		Q2: Float64Array,
		strideQ2: number,
		offsetQ2: number,
		INDX: Int32Array,
		strideINDX: number,
		offsetINDX: number,
		INDXC: Int32Array,
		strideINDXC: number,
		offsetINDXC: number,
		INDXP: Int32Array,
		strideINDXP: number,
		offsetINDXP: number,
		COLTYP: Int32Array,
		strideCOLTYP: number,
		offsetCOLTYP: number
	): Float64Array;
}

/**
* Merge eigenvalues and deflate secular equation in divide and conquer
*/
declare var dlaed2: Routine;

export = dlaed2;
