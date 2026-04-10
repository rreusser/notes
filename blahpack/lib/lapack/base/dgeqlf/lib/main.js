
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgeqlf = require( './dgeqlf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgeqlf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgeqlf;
