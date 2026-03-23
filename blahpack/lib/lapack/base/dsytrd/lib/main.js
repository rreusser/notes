

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsytrd = require( './dsytrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytrd;
