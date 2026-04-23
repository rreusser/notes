

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztplqt2 = require( './ztplqt2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztplqt2, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztplqt2;
