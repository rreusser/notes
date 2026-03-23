

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var ztrti2 = require( './ztrti2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrti2, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrti2;
