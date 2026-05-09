
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgemlqt = require( './dgemlqt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgemlqt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgemlqt;
