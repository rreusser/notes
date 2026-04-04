
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsprfs = require( './dsprfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsprfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsprfs;
