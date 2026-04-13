
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgeqr2p = require( './zgeqr2p.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeqr2p, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeqr2p;
