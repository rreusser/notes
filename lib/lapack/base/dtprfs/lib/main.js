
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtprfs = require( './dtprfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtprfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtprfs;
