'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsyrfs = require( './zsyrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsyrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsyrfs;
