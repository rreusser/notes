
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpprfs = require( './dpprfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpprfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpprfs;
