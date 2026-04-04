
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpprfs = require( './zpprfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpprfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpprfs;
