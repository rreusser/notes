
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsprfs = require( './zsprfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsprfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsprfs;
