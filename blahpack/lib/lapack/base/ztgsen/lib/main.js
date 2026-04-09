
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztgsen = require( './ztgsen.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztgsen, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztgsen;
